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

import play.api.i18n.Messages
import play.api.libs.json.*

import scala.util.Try

object UploadTemplateCsvSchema {

  type CsvRow = Vector[String]

  private val LinesToSkipBeforeSectionRow: Int = 10
  val SectionLineNumber: Int                   = LinesToSkipBeforeSectionRow + 1
  val HeaderLineNumber: Int                    = SectionLineNumber + 2

  private val DataStartLineNumber: Int = HeaderLineNumber + 1
  val SectionRowIndex: Int             = SectionLineNumber - 1
  val HeaderRowIndex: Int              = HeaderLineNumber - 1
  val DataStartIndex: Int              = DataStartLineNumber - 1

  val NotificationSectionIndex = 1
  val CertificateSectionIndex  = 6

  val NotificationSectionExpected = "Notification"
  val CertificateSectionExpected  = "Certificate"

  val ExpectedHeaders: Seq[String] = Seq(
    "Company name",
    "CRN",
    "UTR",
    "Company type (select one)",
    "Company status (select one)",
    "Financial year end (DD/MM/YYYY)",
    "Corporation tax",
    "VAT\n(Value added tax)",
    "PAYE\n(Pay As You Earn)",
    "Insurance premium tax",
    "Stamp duty land tax",
    "Stamp duty reserve tax",
    "Petroleum revenue tax",
    "Customs Duties",
    "Excise Duties",
    "Bank Levy",
    "Certificate type",
    "Your explanation should include:\n- why the SAO provided a qualified certificate\n- what went wrong with the tax accounting arrangements and why, not just a list of errors"
  )

  enum Column(val columnIndex: Int, val messageKey: String) {
    case CompanyName            extends Column(0, "uploadTemplateCsvParser.column.CompanyName")
    case Crn                    extends Column(1, "uploadTemplateCsvParser.column.Crn")
    case Utr                    extends Column(2, "uploadTemplateCsvParser.column.Utr")
    case CompanyType            extends Column(3, "uploadTemplateCsvParser.column.CompanyType")
    case CompanyStatus          extends Column(4, "uploadTemplateCsvParser.column.CompanyStatus")
    case FinancialYearEndDate   extends Column(5, "uploadTemplateCsvParser.column.FinancialYearEndDate")
    case CorporationTax         extends Column(6, "uploadTemplateCsvParser.column.CorporationTax")
    case Vat                    extends Column(7, "uploadTemplateCsvParser.column.Vat")
    case Paye                   extends Column(8, "uploadTemplateCsvParser.column.Paye")
    case InsurancePremiumTax    extends Column(9, "uploadTemplateCsvParser.column.InsurancePremiumTax")
    case StampDutyLandTax       extends Column(10, "uploadTemplateCsvParser.column.StampDutyLandTax")
    case StampDutyReserveTax    extends Column(11, "uploadTemplateCsvParser.column.StampDutyReserveTax")
    case PetroleumRevenueTax    extends Column(12, "uploadTemplateCsvParser.column.PetroleumRevenueTax")
    case CustomsDuties          extends Column(13, "uploadTemplateCsvParser.column.CustomsDuties")
    case ExciseDuties           extends Column(14, "uploadTemplateCsvParser.column.ExciseDuties")
    case BankLevy               extends Column(15, "uploadTemplateCsvParser.column.BankLevy")
    case CertificateType        extends Column(16, "uploadTemplateCsvParser.column.CertificateType")
    case QualificationStatement extends Column(17, "uploadTemplateCsvParser.column.QualificationStatement")
  }

  object Column {
    given Reads[Column] = JsPath
      .read[String]
      .flatMapResult(name =>
        Try(Column.valueOf(name)).fold(_ => JsError("Not a valid Column"), value => JsSuccess(value))
      )

    given Writes[Column] = Writes[Column](r => JsString(r.toString))

    extension (column: Column) {
      def resolve(using messages: Messages): String = messages(column.messageKey)
    }
  }

  enum TemplateError(val messageKey: String) {
    case InvalidTemplateError      extends TemplateError("uploadTemplateCsvParser.error.templateFile")
    case CompanyNameError          extends TemplateError("uploadTemplateCsvParser.error.companyName")
    case UtrError                  extends TemplateError("uploadTemplateCsvParser.error.companyUtr")
    case CrnError                  extends TemplateError("uploadTemplateCsvParser.error.companyCrn")
    case CompanyTypeError          extends TemplateError("uploadTemplateCsvParser.error.companyType")
    case CompanyStatusError        extends TemplateError("uploadTemplateCsvParser.error.companyStatus")
    case FinancialYearEndDateError extends TemplateError("uploadTemplateCsvParser.error.financialYearEndDate")
    case TaxRegimeError            extends TemplateError("uploadTemplateCsvParser.error.taxRegime")
    case CertificateTypeError      extends TemplateError("uploadTemplateCsvParser.error.certificateType")
    case QualificationStatementMissingError
        extends TemplateError("uploadTemplateCsvParser.error.qualificationStatement.missing")
    case QualificationStatementTooLongError
        extends TemplateError("uploadTemplateCsvParser.error.qualificationStatement.tooLong")
    case QualificationStatementProhibitedError
        extends TemplateError("uploadTemplateCsvParser.error.qualificationStatement.prohibited")
  }

  object TemplateError {
    given Reads[TemplateError] = JsPath
      .read[String]
      .flatMapResult(name =>
        Try(TemplateError.valueOf(name)).fold(_ => JsError("Not a valid TemplateError"), value => JsSuccess(value))
      )

    given Writes[TemplateError] = Writes[TemplateError](r => JsString(r.toString))

    extension (err: TemplateError) {
      def resolve(using messages: Messages): String = messages(err.messageKey)
    }
  }

  def cellValue(row: CsvRow, index: Int): String =
    if index < row.length then row(index).trim else ""
}
