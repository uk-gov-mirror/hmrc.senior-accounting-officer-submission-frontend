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

import base.SpecBase
import models.upload.*
import models.upload.TemplateParseResult.{Invalid, Valid}

import java.time.LocalDate

class UploadTemplateCsvParserSpec extends SpecBase {

  private val parser = new UploadTemplateCsvParser()

  private def parsedRow(
      companyName: String,
      companyType: CompanyType,
      companyStatus: CompanyStatus,
      corporationTax: Boolean = true,
      certificateType: Option[CertificateType],
      additionalInformation: Option[String]
  ) =
    ParsedSubmissionRow(
      notification = NotificationFields(
        companyName = companyName,
        companyUtr = CompanyUtr("0123456789"),
        companyCrn = Some(CompanyCrn("12345678")),
        companyType = companyType,
        companyStatus = companyStatus,
        financialYearEndDate = LocalDate.of(2025, 12, 31)
      ),
      certificate = CertificateFields(
        corporationTax = corporationTax,
        valueAddedTax = false,
        paye = false,
        insurancePremiumTax = false,
        stampDutyLandTax = false,
        stampDutyReserveTax = false,
        petroleumRevenueTax = false,
        customsDuties = false,
        exciseDuties = false,
        bankLevy = false,
        certificateType = certificateType,
        additionalInformation = additionalInformation
      )
    )

  private val descriptiveRows: Seq[Seq[String]] =
    (1 to 6).map(index => Seq(s"Descriptive row $index"))

  private val sectionRow: Seq[String] = Seq("", "Notification", "", "", "", "", "Certificate")

  private val validDataRow: Seq[String] = Seq(
    "Test Plc",
    "0123456789",
    "12345678",
    "PLC",
    "Active",
    "31/12/2025",
    "x",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "Qualified",
    "Example additional info"
  )

  private val notificationOnlyDataRow: Seq[String] = Seq(
    "Beta Ltd",
    "0123456789",
    "12345678",
    "LTD",
    "Dormant",
    "31/12/2025",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    ""
  )

  private val blankDataRow: Seq[String] = Seq.fill(UploadTemplateCsvParser.ExpectedHeaders.length)("")

  private def toCsv(rows: Seq[Seq[String]]): String =
    rows.map(row => row.map(escapeCsv).mkString(",")).mkString("\n")

  private def escapeCsv(value: String): String =
    if value.exists(ch => ch == ',' || ch == '\n' || ch == '"') then s"\"${value.replace("\"", "\"\"")}\""
    else value

  "UploadTemplateCsvParser.parse" - {

    "must parse a valid CSV and return typed rows" in {
      val csv = toCsv(
        descriptiveRows ++
          Seq(sectionRow, UploadTemplateCsvParser.ExpectedHeaders, validDataRow)
      )

      val result = parser.parse(csv)

      result mustBe Valid(
        Seq(
          parsedRow(
            companyName = "Test Plc",
            companyType = CompanyType.PLC,
            companyStatus = CompanyStatus.Active,
            certificateType = Some(CertificateType.Qualified),
            additionalInformation = Some("Example additional info")
          )
        )
      )
    }

    "must skip fully blank data rows" in {
      val csv = toCsv(
        descriptiveRows ++
          Seq(sectionRow, UploadTemplateCsvParser.ExpectedHeaders, blankDataRow, validDataRow)
      )

      val result = parser.parse(csv)

      result mustBe Valid(
        Seq(
          parsedRow(
            companyName = "Test Plc",
            companyType = CompanyType.PLC,
            companyStatus = CompanyStatus.Active,
            certificateType = Some(CertificateType.Qualified),
            additionalInformation = Some("Example additional info")
          )
        )
      )
    }

    "must allow notification rows where certificate columns are empty" in {
      val csv = toCsv(
        descriptiveRows ++
          Seq(sectionRow, UploadTemplateCsvParser.ExpectedHeaders, notificationOnlyDataRow)
      )

      val result = parser.parse(csv)

      result mustBe Valid(
        Seq(
          parsedRow(
            companyName = "Beta Ltd",
            companyType = CompanyType.LTD,
            companyStatus = CompanyStatus.Dormant,
            corporationTax = false,
            certificateType = None,
            additionalInformation = None
          )
        )
      )
    }

    "must return errors when the section row does not match template contract" in {
      val csv = toCsv(
        descriptiveRows ++
          Seq(Seq("", "Wrong", "", "", "", "", "AlsoWrong"), UploadTemplateCsvParser.ExpectedHeaders, validDataRow)
      )

      val result = parser.parse(csv)

      result match {
        case Invalid(errors) =>
          errors.count(_.code == "invalid_section_row") mustBe 2
          errors.map(_.line).distinct mustBe Seq(7)
        case _ =>
          fail("Expected parser to fail when section row is invalid")
      }
    }

    "must return errors when headers do not exactly match" in {
      val badHeaders = UploadTemplateCsvParser.ExpectedHeaders.updated(1, "Company UTR BAD")

      val csv = toCsv(
        descriptiveRows ++ Seq(sectionRow, badHeaders, validDataRow)
      )

      val result = parser.parse(csv)

      result match {
        case Invalid(errors) =>
          errors.exists(e => e.code == "header_mismatch" && e.column.contains("Company UTR")) mustBe true
        case _ =>
          fail("Expected parser to fail when headers are invalid")
      }
    }

    "must return row-level validation errors for invalid enums and tax markers" in {
      val badRow = validDataRow
        .updated(3, "INVALID_TYPE")
        .updated(4, "INVALID_STATUS")
        .updated(6, "YES")
        .updated(16, "INVALID_CERT")

      val csv = toCsv(
        descriptiveRows ++ Seq(sectionRow, UploadTemplateCsvParser.ExpectedHeaders, badRow)
      )

      val result = parser.parse(csv)

      result match {
        case Invalid(errors) =>
          errors.map(_.code) must contain allOf (
            "invalid_company_type",
            "invalid_company_status",
            "invalid_tax_regime_value",
            "invalid_certificate_type"
          )
        case _ =>
          fail("Expected parser to fail when row values are invalid")
      }
    }

    "must handle BOM, commas and new lines in quoted fields" in {
      val quotedRow = validDataRow.updated(17, "Line1, with comma\nLine2")

      val csv = "\uFEFF" + toCsv(
        descriptiveRows ++ Seq(sectionRow, UploadTemplateCsvParser.ExpectedHeaders, quotedRow)
      )

      val result = parser.parse(csv)

      result mustBe Valid(
        Seq(
          parsedRow(
            companyName = "Test Plc",
            companyType = CompanyType.PLC,
            companyStatus = CompanyStatus.Active,
            certificateType = Some(CertificateType.Qualified),
            additionalInformation = Some("Line1, with comma\nLine2")
          )
        )
      )
    }
  }
}
