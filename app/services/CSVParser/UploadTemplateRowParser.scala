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

package services.CSVParser

import models.upload.*
import models.upload.TemplateParseResult.{Invalid, Valid}
import services.CSVParser.UploadTemplateCsvSchema.*

import javax.inject.Inject

final case class UploadTemplateRowErrorMessages(
    companyName: String,
    companyUtr: String,
    companyCrn: String,
    companyType: String,
    companyStatus: String,
    financialYearEndDate: String,
    taxRegime: String,
    certificateType: String,
    additionalInformation: String
)

class UploadTemplateRowParser @Inject() (
    companyFieldParser: CompanyFieldParser,
    taxRegimeParser: TaxRegimeParser,
    certificateRulesValidator: CertificateRulesValidator
) {

  private final case class ParsedRowResult(
      row: Option[ParsedSubmissionRow],
      errors: Vector[TemplateParseError]
  )

  def parseDataRows(
      rows: Vector[CsvRow],
      rowErrorMessages: UploadTemplateRowErrorMessages,
      templateFileErrorMessage: String
  ): TemplateParseResult = {
    val rowBuilder   = Vector.newBuilder[ParsedSubmissionRow]
    val errorBuilder = Vector.newBuilder[TemplateParseError]

    rows.iterator.zipWithIndex.drop(DataStartIndex).foreach { case (rawRow, idx) =>
      val rowResult = parseDataRow(rawRow, idx + 1, rowErrorMessages, templateFileErrorMessage)
      rowResult.row.foreach(rowBuilder += _)
      errorBuilder ++= rowResult.errors
    }

    val errors = errorBuilder.result()
    if errors.nonEmpty then Invalid(errors) else Valid(rowBuilder.result())
  }

  private def parseDataRow(
      rawRow: CsvRow,
      lineNumber: Int,
      rowErrorMessages: UploadTemplateRowErrorMessages,
      templateFileErrorMessage: String
  ): ParsedRowResult = {
    val row = normalizedDataColumns(rawRow)

    val extraColumnErrors = Vector.from(
      Option.when(rawRow.drop(ExpectedHeaders.length).exists(_.trim.nonEmpty))(
        TemplateParseError(
          line = lineNumber,
          column = None,
          code = "unexpected_data_columns",
          message = templateFileErrorMessage
        )
      )
    )

    if row.forall(_.isEmpty) then ParsedRowResult(None, extraColumnErrors)
    else {
      val companyResult = companyFieldParser.parse(lineNumber, row, rowErrorMessages)
      val taxResult     = taxRegimeParser.parse(lineNumber, row, rowErrorMessages)
      val certResult    = certificateRulesValidator.parse(
        lineNumber = lineNumber,
        certificateTypeValue = row(CertificateTypeIndex),
        additionalInformationValue = row(AdditionalInformationIndex),
        taxFlags = taxResult.flags,
        rowErrorMessages = rowErrorMessages
      )

      val rowErrors =
        extraColumnErrors ++ companyResult.errors ++ taxResult.errors ++ certResult.errors

      if rowErrors.nonEmpty then ParsedRowResult(None, rowErrors)
      else {
        val parsedRow =
          for {
            company  <- companyResult.fields
            certType <- certResult.certificateType
          } yield ParsedSubmissionRow(
            notification = NotificationFields(
              companyName = company.companyName,
              companyUtr = company.companyUtr,
              companyCrn = company.companyCrn,
              companyType = company.companyType,
              companyStatus = company.companyStatus,
              financialYearEndDate = company.financialYearEndDate
            ),
            certificate = CertificateFields(
              corporationTax = taxResult.flags.corporationTax,
              valueAddedTax = taxResult.flags.valueAddedTax,
              paye = taxResult.flags.paye,
              insurancePremiumTax = taxResult.flags.insurancePremiumTax,
              stampDutyLandTax = taxResult.flags.stampDutyLandTax,
              stampDutyReserveTax = taxResult.flags.stampDutyReserveTax,
              petroleumRevenueTax = taxResult.flags.petroleumRevenueTax,
              customsDuties = taxResult.flags.customsDuties,
              exciseDuties = taxResult.flags.exciseDuties,
              bankLevy = taxResult.flags.bankLevy,
              certificateType = Some(certType),
              additionalInformation = certResult.additionalInformation
            )
          )

        parsedRow match {
          case Some(value) => ParsedRowResult(Some(value), Vector.empty)
          case None        =>
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
  }

  private def normalizedDataColumns(row: CsvRow): IndexedSeq[String] =
    ExpectedHeaders.indices.map(cellValue(row, _))
}
