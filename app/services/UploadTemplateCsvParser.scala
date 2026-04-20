/*
 * Copyright 2026 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package services

import com.univocity.parsers.csv.{CsvParser, CsvParserSettings}
import models.upload.*
import models.upload.TemplateParseResult.{Invalid, Valid}

import scala.jdk.CollectionConverters.*
import scala.util.Try

import java.io.StringReader
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import java.time.format.ResolverStyle
import javax.inject.Inject

import UploadTemplateCsvParser.*

class UploadTemplateCsvParser @Inject() {

  def parse(csv: String): TemplateParseResult = {
    Try {
      val rows   = parseCsvRows(csv)
      val errors =
        validateSectionRow(rows.lift(SectionRowIndex)) ++
          validateHeaderRow(rows.lift(HeaderRowIndex))

      errors match {
        case Nil      => parseDataRows(rows)
        case nonEmpty => Invalid(nonEmpty)
      }
    }.fold(
      err =>
        Invalid(
          Seq(
            TemplateParseError(
              0,
              None,
              "invalid_csv",
              s"Unable to parse CSV content: ${err.getMessage}"
            )
          )
        ),
      identity
    )
  }

  private def parseCsvRows(csv: String): Vector[CsvRow] = {
    val settings = CsvParserSettings()
    settings.setLineSeparatorDetectionEnabled(true)
    settings.setReadInputOnSeparateThread(false)
    settings.setNullValue("")
    settings.setEmptyValue("")
    settings.setMaxColumns(64)
    settings.setMaxCharsPerColumn(100000)

    val parser    = CsvParser(settings)
    val sanitized = csv.stripPrefix("\uFEFF")

    parser.parseAll(StringReader(sanitized)).asScala.iterator.map(_.toVector).toVector
  }

  private def validateSectionRow(rowOpt: Option[CsvRow]): Seq[TemplateParseError] = {
    rowOpt match {
      case None =>
        Seq(
          TemplateParseError(
            line = SectionLineNumber,
            column = None,
            code = "missing_section_row",
            message = s"Line $SectionLineNumber is missing section headers."
          )
        )
      case Some(row) =>
        val notificationSection = cellValue(row, NotificationSectionIndex)
        val certificateSection  = cellValue(row, CertificateSectionIndex)

        Seq(
          Option.when(notificationSection != NotificationSectionExpected)(
            TemplateParseError(
              line = SectionLineNumber,
              column = Some("Notification"),
              code = "invalid_section_row",
              message = s"Line $SectionLineNumber must contain '$NotificationSectionExpected' in column B."
            )
          ),
          Option.when(certificateSection != CertificateSectionExpected)(
            TemplateParseError(
              line = SectionLineNumber,
              column = Some("Certificate"),
              code = "invalid_section_row",
              message = s"Line $SectionLineNumber must contain '$CertificateSectionExpected' in column G."
            )
          )
        ).flatten
    }
  }

  private def validateHeaderRow(rowOpt: Option[CsvRow]): Seq[TemplateParseError] = {
    rowOpt match {
      case None =>
        Seq(
          TemplateParseError(
            line = HeaderLineNumber,
            column = None,
            code = "missing_header_row",
            message = s"Line $HeaderLineNumber is missing the template table headers."
          )
        )
      case Some(row) =>
        val extraColumnError = Option.when(row.drop(ExpectedHeaders.length).exists(_.trim.nonEmpty))(
          TemplateParseError(
            line = HeaderLineNumber,
            column = None,
            code = "unexpected_header_columns",
            message = s"Line $HeaderLineNumber contains more than ${ExpectedHeaders.length} populated columns."
          )
        )

        val headerErrors = ExpectedHeaders.zipWithIndex.collect {
          case (expectedHeader, idx) if cellValue(row, idx) != expectedHeader =>
            val actualHeader = cellValue(row, idx)
            TemplateParseError(
              line = HeaderLineNumber,
              column = Some(expectedHeader),
              code = "header_mismatch",
              message =
                s"Line $HeaderLineNumber column ${idx + 1} expected '$expectedHeader' but found '$actualHeader'."
            )
        }

        extraColumnError.toSeq ++ headerErrors
    }
  }

  private def parseDataRows(rows: Vector[CsvRow]): TemplateParseResult = {
    val rowBuilder   = Vector.newBuilder[ParsedSubmissionRow]
    val errorBuilder = Vector.newBuilder[TemplateParseError]

    rows.iterator.zipWithIndex.drop(DataStartIndex).foreach { case (rawRow, idx) =>
      val rowResult = parseDataRow(rawRow, idx + 1)
      rowResult.row.foreach(rowBuilder += _)
      errorBuilder ++= rowResult.errors
    }

    val errors = errorBuilder.result()

    if errors.nonEmpty then Invalid(errors)
    else Valid(rowBuilder.result())
  }

  private def parseDataRow(rawRow: CsvRow, lineNumber: Int): ParsedRowResult = {
    val row = normalizedDataColumns(rawRow)

    val extraColumnErrors = Vector.from(
      Option.when(rawRow.drop(ExpectedHeaders.length).exists(_.trim.nonEmpty))(
        TemplateParseError(
          line = lineNumber,
          column = None,
          code = "unexpected_data_columns",
          message = s"Line $lineNumber has values beyond column ${ExpectedHeaders.length}."
        )
      )
    )

    if row.forall(_.isEmpty) then ParsedRowResult(None, extraColumnErrors)
    else {
      val requiredErrors = RequiredNotificationColumnIndexes.collect {
        case index if row(index).isEmpty =>
          TemplateParseError(
            line = lineNumber,
            column = Some(ExpectedHeaders(index)),
            code = "missing_required_value",
            message = s"Line $lineNumber ${ExpectedHeaders(index)} is required."
          )
      }

      val (companyType, companyTypeErrors) = parseEnumValue(
        lineNumber,
        CompanyTypeIndex,
        row(CompanyTypeIndex),
        CompanyType.fromString,
        "invalid_company_type"
      )

      val (companyStatus, companyStatusErrors) = parseEnumValue(
        lineNumber,
        CompanyStatusIndex,
        row(CompanyStatusIndex),
        CompanyStatus.fromString,
        "invalid_company_status"
      )

      val (certificateType, certificateTypeErrors) = parseEnumValue(
        lineNumber,
        CertificateTypeIndex,
        row(CertificateTypeIndex),
        CertificateType.fromString,
        "invalid_certificate_type"
      )

      val (companyUtr, companyUtrErrors) =
        parseCompanyUtrValue(lineNumber, row(CompanyUtrIndex))
      val (companyCrn, companyCrnErrors) =
        parseCompanyCrnValue(lineNumber, row(CompanyCrnIndex))
      val (financialYearEndDate, financialYearEndDateErrors) =
        parseFinancialYearEndDateValue(lineNumber, row(FinancialYearEndDateIndex))

      val (taxFlags, taxRegimeErrors) = parseTaxFlags(lineNumber, row)

      val rowErrors =
        List(
          extraColumnErrors,
          requiredErrors,
          companyUtrErrors,
          companyCrnErrors,
          financialYearEndDateErrors,
          companyTypeErrors,
          companyStatusErrors,
          certificateTypeErrors,
          taxRegimeErrors
        ).iterator.flatten.toVector

      if rowErrors.nonEmpty then ParsedRowResult(None, rowErrors)
      else
        (companyUtr, companyType, companyStatus, financialYearEndDate) match {
          case (Some(utr), Some(ct), Some(cs), Some(fyeDate)) =>
            ParsedRowResult(
              row = Some(
                ParsedSubmissionRow(
                  notification = NotificationFields(
                    companyName = row(CompanyNameIndex),
                    companyUtr = utr,
                    companyCrn = companyCrn,
                    companyType = ct,
                    companyStatus = cs,
                    financialYearEndDate = fyeDate
                  ),
                  certificate = CertificateFields(
                    corporationTax = taxFlags.corporationTax,
                    valueAddedTax = taxFlags.valueAddedTax,
                    paye = taxFlags.paye,
                    insurancePremiumTax = taxFlags.insurancePremiumTax,
                    stampDutyLandTax = taxFlags.stampDutyLandTax,
                    stampDutyReserveTax = taxFlags.stampDutyReserveTax,
                    petroleumRevenueTax = taxFlags.petroleumRevenueTax,
                    customsDuties = taxFlags.customsDuties,
                    exciseDuties = taxFlags.exciseDuties,
                    bankLevy = taxFlags.bankLevy,
                    certificateType = certificateType,
                    additionalInformation = Option(row(AdditionalInformationIndex)).filter(_.nonEmpty)
                  )
                )
              ),
              errors = Vector.empty
            )
          case _ =>
            ParsedRowResult(
              row = None,
              errors = Vector(
                TemplateParseError(
                  line = lineNumber,
                  column = None,
                  code = "internal_parser_error",
                  message = s"Line $lineNumber could not be parsed due to an internal parser state mismatch."
                )
              )
            )
        }
    }
  }

  private def normalizedDataColumns(row: CsvRow): IndexedSeq[String] =
    ExpectedHeaders.indices.map(cellValue(row, _))

  private def parseEnumValue[T](
      lineNumber: Int,
      columnIndex: Int,
      value: String,
      parse: String => Option[T],
      code: String
  ): (Option[T], Vector[TemplateParseError]) =
    if value.isEmpty then (None, Vector.empty)
    else
      parse(value)
        .map(parsed => (Some(parsed), Vector.empty))
        .getOrElse(
          (
            None,
            Vector(
              TemplateParseError(
                line = lineNumber,
                column = Some(ExpectedHeaders(columnIndex)),
                code = code,
                message = s"Line $lineNumber ${ExpectedHeaders(columnIndex)} value '$value' is not valid."
              )
            )
          )
        )

  private def parseTaxRegimeValue(
      lineNumber: Int,
      columnIndex: Int,
      value: String
  ): (Boolean, Vector[TemplateParseError]) =
    value.toLowerCase match {
      case ""  => (false, Vector.empty)
      case "x" => (true, Vector.empty)
      case _   =>
        (
          false,
          Vector(
            TemplateParseError(
              line = lineNumber,
              column = Some(ExpectedHeaders(columnIndex)),
              code = "invalid_tax_regime_value",
              message = s"Line $lineNumber ${ExpectedHeaders(columnIndex)} must be 'x' or blank."
            )
          )
        )
    }

  private def parseCompanyUtrValue(
      lineNumber: Int,
      value: String
  ): (Option[CompanyUtr], Vector[TemplateParseError]) =
    if value.isEmpty then (None, Vector.empty)
    else
      CompanyUtr
        .fromString(value)
        .map(parsed => (Some(parsed), Vector.empty))
        .getOrElse(
          (
            None,
            Vector(
              TemplateParseError(
                line = lineNumber,
                column = Some(ExpectedHeaders(CompanyUtrIndex)),
                code = "invalid_company_utr",
                message = s"Line $lineNumber ${ExpectedHeaders(CompanyUtrIndex)} must be a 10 digit number."
              )
            )
          )
        )

  private def parseCompanyCrnValue(
      lineNumber: Int,
      value: String
  ): (Option[CompanyCrn], Vector[TemplateParseError]) =
    if value.isEmpty then (None, Vector.empty)
    else
      CompanyCrn
        .fromString(value)
        .map(parsed => (Some(parsed), Vector.empty))
        .getOrElse(
          (
            None,
            Vector(
              TemplateParseError(
                line = lineNumber,
                column = Some(ExpectedHeaders(CompanyCrnIndex)),
                code = "invalid_company_crn",
                message = s"Line $lineNumber ${ExpectedHeaders(CompanyCrnIndex)} must be 1 to 8 letters or numbers."
              )
            )
          )
        )

  private def parseFinancialYearEndDateValue(
      lineNumber: Int,
      value: String
  ): (Option[LocalDate], Vector[TemplateParseError]) =
    if value.isEmpty then (None, Vector.empty)
    else
      try (Some(LocalDate.parse(value, FinancialYearEndDateFormatter)), Vector.empty)
      catch {
        case _: DateTimeParseException =>
          (
            None,
            Vector(
              TemplateParseError(
                line = lineNumber,
                column = Some(ExpectedHeaders(FinancialYearEndDateIndex)),
                code = "invalid_financial_year_end_date",
                message =
                  s"Line $lineNumber ${ExpectedHeaders(FinancialYearEndDateIndex)} must be in dd/MM/yyyy format."
              )
            )
          )
      }

  private def parseTaxFlags(
      lineNumber: Int,
      row: IndexedSeq[String]
  ): (TaxFlags, Vector[TemplateParseError]) = {
    val (corporationTax, corporationTaxErrors) =
      parseTaxRegimeValue(lineNumber, CorporationTaxIndex, row(CorporationTaxIndex))
    val (valueAddedTax, valueAddedTaxErrors) =
      parseTaxRegimeValue(lineNumber, ValueAddedTaxIndex, row(ValueAddedTaxIndex))
    val (paye, payeErrors) =
      parseTaxRegimeValue(lineNumber, PayeIndex, row(PayeIndex))
    val (insurancePremiumTax, insurancePremiumTaxErrors) =
      parseTaxRegimeValue(lineNumber, InsurancePremiumTaxIndex, row(InsurancePremiumTaxIndex))
    val (stampDutyLandTax, stampDutyLandTaxErrors) =
      parseTaxRegimeValue(lineNumber, StampDutyLandTaxIndex, row(StampDutyLandTaxIndex))
    val (stampDutyReserveTax, stampDutyReserveTaxErrors) =
      parseTaxRegimeValue(lineNumber, StampDutyReserveTaxIndex, row(StampDutyReserveTaxIndex))
    val (petroleumRevenueTax, petroleumRevenueTaxErrors) =
      parseTaxRegimeValue(lineNumber, PetroleumRevenueTaxIndex, row(PetroleumRevenueTaxIndex))
    val (customsDuties, customsDutiesErrors) =
      parseTaxRegimeValue(lineNumber, CustomsDutiesIndex, row(CustomsDutiesIndex))
    val (exciseDuties, exciseDutiesErrors) =
      parseTaxRegimeValue(lineNumber, ExciseDutiesIndex, row(ExciseDutiesIndex))
    val (bankLevy, bankLevyErrors) =
      parseTaxRegimeValue(lineNumber, BankLevyIndex, row(BankLevyIndex))

    val errors =
      List(
        corporationTaxErrors,
        valueAddedTaxErrors,
        payeErrors,
        insurancePremiumTaxErrors,
        stampDutyLandTaxErrors,
        stampDutyReserveTaxErrors,
        petroleumRevenueTaxErrors,
        customsDutiesErrors,
        exciseDutiesErrors,
        bankLevyErrors
      ).iterator.flatten.toVector

    (
      TaxFlags(
        corporationTax,
        valueAddedTax,
        paye,
        insurancePremiumTax,
        stampDutyLandTax,
        stampDutyReserveTax,
        petroleumRevenueTax,
        customsDuties,
        exciseDuties,
        bankLevy
      ),
      errors
    )
  }

  private def cellValue(row: CsvRow, index: Int): String =
    if index < row.length then row(index).trim else ""

}

object UploadTemplateCsvParser {

  private type CsvRow = Vector[String]

  private final case class ParsedRowResult(
      row: Option[ParsedSubmissionRow],
      errors: Vector[TemplateParseError]
  )

  private final case class TaxFlags(
      corporationTax: Boolean,
      valueAddedTax: Boolean,
      paye: Boolean,
      insurancePremiumTax: Boolean,
      stampDutyLandTax: Boolean,
      stampDutyReserveTax: Boolean,
      petroleumRevenueTax: Boolean,
      customsDuties: Boolean,
      exciseDuties: Boolean,
      bankLevy: Boolean
  )

  private val LinesToSkipBeforeSectionRow = 6
  private val SectionLineNumber           = LinesToSkipBeforeSectionRow + 1
  private val HeaderLineNumber            = SectionLineNumber + 1
  private val DataStartLineNumber         = HeaderLineNumber + 1

  private val SectionRowIndex = SectionLineNumber - 1
  private val HeaderRowIndex  = HeaderLineNumber - 1
  private val DataStartIndex  = DataStartLineNumber - 1

  private val NotificationSectionIndex = 1
  private val CertificateSectionIndex  = 6

  private val NotificationSectionExpected = "Notification"
  private val CertificateSectionExpected  = "Certificate"

  val ExpectedHeaders: Seq[String] = Seq(
    "Company name",
    "Company UTR",
    "Company CRN",
    "Company type",
    "Company status",
    "Financial year end date",
    "Corporation tax",
    "Value added tax",
    "PAYE",
    "Insurance premium tax",
    "Stamp duty land tax",
    "Stamp duty reserve tax",
    "Petroleum revenue tax",
    "Customs Duties",
    "Excise Duties",
    "Bank Levy",
    "Certificate type",
    "Additional information"
  )

  private val CompanyNameIndex          = 0
  private val CompanyUtrIndex           = 1
  private val CompanyCrnIndex           = 2
  private val CompanyTypeIndex          = 3
  private val CompanyStatusIndex        = 4
  private val FinancialYearEndDateIndex = 5

  private val CorporationTaxIndex        = 6
  private val ValueAddedTaxIndex         = 7
  private val PayeIndex                  = 8
  private val InsurancePremiumTaxIndex   = 9
  private val StampDutyLandTaxIndex      = 10
  private val StampDutyReserveTaxIndex   = 11
  private val PetroleumRevenueTaxIndex   = 12
  private val CustomsDutiesIndex         = 13
  private val ExciseDutiesIndex          = 14
  private val BankLevyIndex              = 15
  private val CertificateTypeIndex       = 16
  private val AdditionalInformationIndex = 17

  private val FinancialYearEndDateFormatter =
    DateTimeFormatter.ofPattern("dd/MM/yyyy").withResolverStyle(ResolverStyle.SMART)

  private val RequiredNotificationColumnIndexes = Seq(
    CompanyNameIndex,
    CompanyUtrIndex,
    CompanyTypeIndex,
    CompanyStatusIndex,
    FinancialYearEndDateIndex
  )
}
