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

package services.csvparser

import models.upload.*
import models.upload.TemplateParseResult.{Invalid, Valid}
import services.csvparser.UploadTemplateCsvSchema.*

import javax.inject.Inject

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
      notificationOnly: Boolean
  ): TemplateParseResult = {
    val parsedRows =
      rows.zipWithIndex.drop(DataStartIndex).map { case (rawRow, idx) =>
        parseDataRow(rawRow, idx + 1, notificationOnly)
      }

    val errors = parsedRows.flatMap(_.errors)

    if errors.nonEmpty then Invalid(errors) else Valid(parsedRows.flatMap(_.row))
  }

  private def parseDataRow(
      rawRow: CsvRow,
      lineNumber: Int,
      notificationOnly: Boolean
  ): ParsedRowResult = {
    val row = normalizedDataColumns(rawRow)

    if row.forall(_.isEmpty) then ParsedRowResult(None, Vector.empty)
    else {
      val companyResult = companyFieldParser.parse(lineNumber, row)
      val taxResult     = taxRegimeParser.parse(lineNumber, row)
      val certResult    =
        if notificationOnly then
          CertificateParseResult(certificateType = None, qualificationStatement = None, errors = Vector.empty)
        else
          certificateRulesValidator.parse(
            lineNumber = lineNumber,
            certificateTypeValue = row(Column.CertificateType.columnIndex),
            qualificationStatementValue = row(Column.QualificationStatement.columnIndex),
            taxFlags = taxResult.flags
          )

      val rowErrors =
        companyResult.errors ++
          (if notificationOnly then Seq.empty else taxResult.errors) ++
          certResult.errors

      if rowErrors.nonEmpty then ParsedRowResult(None, rowErrors)
      else
        val parsedRow = {
          if notificationOnly then
            for {
              company <- companyResult.fields
            } yield ParsedSubmissionRow(
              notification = NotificationFields(
                companyName = company.companyName,
                companyUtr = company.companyUtr,
                companyCrn = company.companyCrn,
                companyType = company.companyType,
                companyStatus = company.companyStatus,
                financialYearEndDate = company.financialYearEndDate
              ),
              certificate = None
            )
          else
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
              certificate = Some(
                CertificateFields(
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
                  qualificationStatement = certResult.qualificationStatement
                )
              )
            )
        }

        ParsedRowResult(row = parsedRow, errors = Vector.empty)
    }
  }

  private def normalizedDataColumns(row: CsvRow): IndexedSeq[String] =
    ExpectedHeaders.indices.map(cellValue(row, _))
}
