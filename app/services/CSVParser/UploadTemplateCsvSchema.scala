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

import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatterBuilder
import java.time.format.ResolverStyle
import java.time.temporal.ChronoField

object UploadTemplateCsvSchema {

  type CsvRow = Vector[String]

  private val LinesToSkipBeforeSectionRow = 6
  val SectionLineNumber: Int              = LinesToSkipBeforeSectionRow + 1
  val HeaderLineNumber: Int               = SectionLineNumber + 1

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

  val CompanyNameIndex          = 0
  val CompanyUtrIndex           = 1
  val CompanyCrnIndex           = 2
  val CompanyTypeIndex          = 3
  val CompanyStatusIndex        = 4
  val FinancialYearEndDateIndex = 5

  val CorporationTaxIndex        = 6
  val ValueAddedTaxIndex         = 7
  val PayeIndex                  = 8
  val InsurancePremiumTaxIndex   = 9
  val StampDutyLandTaxIndex      = 10
  val StampDutyReserveTaxIndex   = 11
  val PetroleumRevenueTaxIndex   = 12
  val CustomsDutiesIndex         = 13
  val ExciseDutiesIndex          = 14
  val BankLevyIndex              = 15
  val CertificateTypeIndex       = 16
  val AdditionalInformationIndex = 17

  val FinancialYearEndDateFormatter: DateTimeFormatter =
    DateTimeFormatterBuilder()
      .appendPattern("dd/MM/yyyy")
      .parseDefaulting(ChronoField.ERA, 1)
      .toFormatter
      .withResolverStyle(ResolverStyle.STRICT)

  val CompanyNameRegex   = "^[A-Za-z0-9 &\\-\\.'’]{1,105}$"
  val CompanyStatusRegex = "^[A-Za-z]{1,15}$"

  val TemplateFileErrorMessageKey =
    "uploadTemplateCsvParser.error.templateFile"
  val CompanyNameErrorMessageKey =
    "uploadTemplateCsvParser.error.companyName"
  val CompanyUtrErrorMessageKey =
    "uploadTemplateCsvParser.error.companyUtr"
  val CompanyCrnErrorMessageKey =
    "uploadTemplateCsvParser.error.companyCrn"
  val CompanyTypeErrorMessageKey =
    "uploadTemplateCsvParser.error.companyType"
  val CompanyStatusErrorMessageKey =
    "uploadTemplateCsvParser.error.companyStatus"
  val FinancialYearEndDateErrorMessageKey =
    "uploadTemplateCsvParser.error.financialYearEndDate"
  val TaxRegimeErrorMessageKey =
    "uploadTemplateCsvParser.error.taxRegime"
  val CertificateTypeErrorMessageKey =
    "uploadTemplateCsvParser.error.certificateType"
  val AdditionalInformationErrorMessageKey =
    "uploadTemplateCsvParser.error.additionalInformation"

  def cellValue(row: CsvRow, index: Int): String =
    if index < row.length then row(index).trim else ""
}
