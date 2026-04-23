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
import java.time.format.DateTimeFormatterBuilder
import java.time.format.DateTimeParseException
import java.time.format.ResolverStyle
import java.time.temporal.ChronoField
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
            message = TemplateFileErrorMessage
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
              message = TemplateFileErrorMessage
            )
          ),
          Option.when(certificateSection != CertificateSectionExpected)(
            TemplateParseError(
              line = SectionLineNumber,
              column = Some("Certificate"),
              code = "invalid_section_row",
              message = TemplateFileErrorMessage
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
            message = TemplateFileErrorMessage
          )
        )
      case Some(row) =>
        val extraColumnError = Option.when(row.drop(ExpectedHeaders.length).exists(_.trim.nonEmpty))(
          TemplateParseError(
            line = HeaderLineNumber,
            column = None,
            code = "unexpected_header_columns",
            message = TemplateFileErrorMessage
          )
        )

        val headerErrors = ExpectedHeaders.zipWithIndex.collect {
          case (expectedHeader, idx) if cellValue(row, idx) != expectedHeader =>
            TemplateParseError(
              line = HeaderLineNumber,
              column = Some(expectedHeader),
              code = "header_mismatch",
              message = TemplateFileErrorMessage
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
          message = TemplateFileErrorMessage
        )
      )
    )

    if row.forall(_.isEmpty) then ParsedRowResult(None, extraColumnErrors)
    else {
      val (companyName, companyNameErrors) =
        parseCompanyNameValue(lineNumber, row(CompanyNameIndex))
      val (companyUtr, companyUtrErrors) =
        parseCompanyUtrValue(lineNumber, row(CompanyUtrIndex))
      val (companyCrn, companyCrnErrors) =
        parseCompanyCrnValue(lineNumber, row(CompanyCrnIndex))
      val (companyType, companyTypeErrors) =
        parseCompanyTypeValue(lineNumber, row(CompanyTypeIndex))
      val (companyStatus, companyStatusErrors) =
        parseCompanyStatusValue(lineNumber, row(CompanyStatusIndex))
      val (financialYearEndDate, financialYearEndDateErrors) =
        parseFinancialYearEndDateValue(lineNumber, row(FinancialYearEndDateIndex))
      val (taxFlags, taxRegimeErrors) =
        parseTaxFlags(lineNumber, row)
      val (certificateType, certificateTypeErrors) =
        parseCertificateTypeValue(lineNumber, row(CertificateTypeIndex), taxFlags)
      val additionalInformationErrors =
        validateAdditionalInformationValue(lineNumber, row(AdditionalInformationIndex), taxFlags.hasAnySelected)

      val rowErrors =
        List(
          extraColumnErrors,
          companyNameErrors,
          companyUtrErrors,
          companyCrnErrors,
          companyTypeErrors,
          companyStatusErrors,
          financialYearEndDateErrors,
          taxRegimeErrors,
          certificateTypeErrors,
          additionalInformationErrors
        ).iterator.flatten.toVector

      if rowErrors.nonEmpty then ParsedRowResult(None, rowErrors)
      else
        (companyName, companyUtr, companyType, companyStatus, financialYearEndDate, certificateType) match {
          case (Some(name), Some(utr), Some(ct), Some(cs), Some(fyeDate), Some(certType)) =>
            ParsedRowResult(
              row = Some(
                ParsedSubmissionRow(
                  notification = NotificationFields(
                    companyName = name,
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
                    certificateType = Some(certType),
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

  private def parseCompanyNameValue(
      lineNumber: Int,
      value: String
  ): (Option[String], Vector[TemplateParseError]) =
    Option(value).filter(_.matches(CompanyNameRegex)).filter(_.length <= 105) match {
      case Some(validName) =>
        (Some(validName), Vector.empty)
      case None =>
        (
          None,
          Vector(
            TemplateParseError(
              line = lineNumber,
              column = Some(ExpectedHeaders(CompanyNameIndex)),
              code = "invalid_company_name",
              message = CompanyNameErrorMessage
            )
          )
        )
    }

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
              message = TaxRegimeErrorMessage
            )
          )
        )
    }

  private def parseCompanyUtrValue(
      lineNumber: Int,
      value: String
  ): (Option[CompanyUtr], Vector[TemplateParseError]) =
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
              message = CompanyUtrErrorMessage
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
                message = CompanyCrnErrorMessage
              )
            )
          )
        )

  private def parseCompanyTypeValue(
      lineNumber: Int,
      value: String
  ): (Option[CompanyType], Vector[TemplateParseError]) =
    CompanyType
      .fromString(value)
      .filter(ct => ct == CompanyType.PLC || ct == CompanyType.LTD)
      .map(parsed => (Some(parsed), Vector.empty))
      .getOrElse(
        (
          None,
          Vector(
            TemplateParseError(
              line = lineNumber,
              column = Some(ExpectedHeaders(CompanyTypeIndex)),
              code = "invalid_company_type",
              message = CompanyTypeErrorMessage
            )
          )
        )
      )

  private def parseCompanyStatusValue(
      lineNumber: Int,
      value: String
  ): (Option[CompanyStatus], Vector[TemplateParseError]) =
    CompanyStatus
      .fromString(value)
      .filter(_ => value.matches(CompanyStatusRegex))
      .map(parsed => (Some(parsed), Vector.empty))
      .getOrElse(
        (
          None,
          Vector(
            TemplateParseError(
              line = lineNumber,
              column = Some(ExpectedHeaders(CompanyStatusIndex)),
              code = "invalid_company_status",
              message = CompanyStatusErrorMessage
            )
          )
        )
      )

  private def parseFinancialYearEndDateValue(
      lineNumber: Int,
      value: String
  ): (Option[LocalDate], Vector[TemplateParseError]) =
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
              message = FinancialYearEndDateErrorMessage
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

  private def parseCertificateTypeValue(
      lineNumber: Int,
      value: String,
      taxFlags: TaxFlags
  ): (Option[CertificateType], Vector[TemplateParseError]) = {
    val parsedFromValue =
      if value.isEmpty then None
      else CertificateType.fromString(value)

    val certificateType =
      parsedFromValue.orElse(Option.when(value.isEmpty && taxFlags.hasAnySelected)(CertificateType.Qualified))

    val isInvalidMissingOrUnknown =
      (value.nonEmpty && parsedFromValue.isEmpty) || (value.isEmpty && !taxFlags.hasAnySelected)

    val hasCrossFieldMismatch =
      certificateType.exists(_ == CertificateType.Unqualified) && taxFlags.hasAnySelected ||
        certificateType.exists(_ == CertificateType.Qualified) && !taxFlags.hasAnySelected

    if isInvalidMissingOrUnknown || hasCrossFieldMismatch then {
      (
        None,
        Vector(
          TemplateParseError(
            line = lineNumber,
            column = Some(ExpectedHeaders(CertificateTypeIndex)),
            code = "invalid_certificate_type",
            message = CertificateTypeErrorMessage
          )
        )
      )
    } else {
      (certificateType, Vector.empty)
    }
  }

  private def validateAdditionalInformationValue(
      lineNumber: Int,
      value: String,
      hasAnyTaxRegimeSelected: Boolean
  ): Vector[TemplateParseError] =
    if hasAnyTaxRegimeSelected && value.isEmpty then {
      Vector(
        TemplateParseError(
          line = lineNumber,
          column = Some(ExpectedHeaders(AdditionalInformationIndex)),
          code = "missing_qualified_reason",
          message = AdditionalInformationErrorMessage
        )
      )
    } else {
      Vector.empty
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
  ) {
    val hasAnySelected: Boolean =
      corporationTax || valueAddedTax || paye || insurancePremiumTax || stampDutyLandTax ||
        stampDutyReserveTax || petroleumRevenueTax || customsDuties || exciseDuties || bankLevy
  }

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
    DateTimeFormatterBuilder()
      .appendPattern("dd/MM/yyyy")
      .parseDefaulting(ChronoField.ERA, 1)
      .toFormatter
      .withResolverStyle(ResolverStyle.STRICT)

  private val CompanyNameRegex   = "^[A-Za-z0-9 &\\-\\.'’]{1,105}$"
  private val CompanyStatusRegex = "^[A-Za-z]{1,15}$"

  private val TemplateFileErrorMessage =
    "The selected file must use the template"
  private val CompanyNameErrorMessage =
    "Enter a valid company name. Maximum 105 characters."
  private val CompanyUtrErrorMessage =
    "Enter a valid Company UTR. It must be 10 digits long."
  private val CompanyCrnErrorMessage =
    "Enter a valid Company Registration Number (CRN). It must be 8 characters"
  private val CompanyTypeErrorMessage =
    "Select or enter a valid company type, for example PLC or LTD"
  private val CompanyStatusErrorMessage =
    "Select or enter a valid company status, for example Active, Administration, Liquidation, Dormant"
  private val FinancialYearEndDateErrorMessage =
    "Enter a valid financial year end date in the format DD/MM/YYYY."
  private val TaxRegimeErrorMessage =
    "Enter 'x' if the company is qualified for this tax regime, or leave blank if unqualified."
  private val CertificateTypeErrorMessage =
    "Only 'unqualified' or 'qualified' can be entered in this field. 'Qualified' will be added automatically if you enter an 'x' against a tax regime in columns G to P"
  private val AdditionalInformationErrorMessage =
    "Explain why the certificate is qualified and a tax regime has been selected."
}
