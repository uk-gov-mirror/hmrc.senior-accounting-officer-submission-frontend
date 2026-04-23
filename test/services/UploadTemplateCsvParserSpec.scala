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
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import services.CSVParser.UploadTemplateCsvParser

import java.time.LocalDate

class UploadTemplateCsvParserSpec extends SpecBase with GuiceOneAppPerSuite {

  private val parser = app.injector.instanceOf[UploadTemplateCsvParser]

  private def parsedRow(
      companyName: String,
      companyType: CompanyType,
      companyStatus: CompanyStatus,
      corporationTax: Boolean = true,
      certificateType: CertificateType,
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
        certificateType = Some(certificateType),
        additionalInformation = additionalInformation
      )
    )

  private val descriptiveRows: Seq[Seq[String]] =
    (1 to 6).map(index => Seq(s"Descriptive row $index"))

  private val sectionRow: Seq[String] = Seq("", "Notification", "", "", "", "", "Certificate")

  private val validQualifiedDataRow: Seq[String] = Seq(
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

  private val validUnqualifiedDataRow: Seq[String] = Seq(
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
    "UnQualified",
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
          Seq(sectionRow, UploadTemplateCsvParser.ExpectedHeaders, validQualifiedDataRow)
      )

      val result = parser.parse(csv)

      result mustBe Valid(
        Seq(
          parsedRow(
            companyName = "Test Plc",
            companyType = CompanyType.PLC,
            companyStatus = CompanyStatus.Active,
            certificateType = CertificateType.Qualified,
            additionalInformation = Some("Example additional info")
          )
        )
      )
    }

    "must skip fully blank data rows" in {
      val csv = toCsv(
        descriptiveRows ++
          Seq(sectionRow, UploadTemplateCsvParser.ExpectedHeaders, blankDataRow, validQualifiedDataRow)
      )

      val result = parser.parse(csv)

      result mustBe Valid(
        Seq(
          parsedRow(
            companyName = "Test Plc",
            companyType = CompanyType.PLC,
            companyStatus = CompanyStatus.Active,
            certificateType = CertificateType.Qualified,
            additionalInformation = Some("Example additional info")
          )
        )
      )
    }

    "must parse an unqualified row when no tax regimes are selected" in {
      val csv = toCsv(
        descriptiveRows ++
          Seq(sectionRow, UploadTemplateCsvParser.ExpectedHeaders, validUnqualifiedDataRow)
      )

      val result = parser.parse(csv)

      result mustBe Valid(
        Seq(
          parsedRow(
            companyName = "Beta Ltd",
            companyType = CompanyType.LTD,
            companyStatus = CompanyStatus.Dormant,
            corporationTax = false,
            certificateType = CertificateType.Unqualified,
            additionalInformation = None
          )
        )
      )
    }

    "must auto-set certificate type to qualified when tax regimes are marked and certificate type is blank" in {
      val autoQualifiedRow = validQualifiedDataRow.updated(16, "")

      val csv = toCsv(
        descriptiveRows ++ Seq(sectionRow, UploadTemplateCsvParser.ExpectedHeaders, autoQualifiedRow)
      )

      val result = parser.parse(csv)

      result mustBe Valid(
        Seq(
          parsedRow(
            companyName = "Test Plc",
            companyType = CompanyType.PLC,
            companyStatus = CompanyStatus.Active,
            certificateType = CertificateType.Qualified,
            additionalInformation = Some("Example additional info")
          )
        )
      )
    }

    "must return errors when the section row does not match template contract" in {
      val csv = toCsv(
        descriptiveRows ++
          Seq(
            Seq("", "Wrong", "", "", "", "", "AlsoWrong"),
            UploadTemplateCsvParser.ExpectedHeaders,
            validQualifiedDataRow
          )
      )

      val result = parser.parse(csv)

      result match {
        case Invalid(errors) =>
          errors.count(_.code == "invalid_section_row") mustBe 2
          errors.forall(_.message == "The selected file must use the template") mustBe true
          errors.map(_.line).distinct mustBe Seq(7)
        case _ =>
          fail("Expected parser to fail when section row is invalid")
      }
    }

    "must return errors when headers do not exactly match" in {
      val badHeaders = UploadTemplateCsvParser.ExpectedHeaders.updated(1, "Company UTR BAD")

      val csv = toCsv(
        descriptiveRows ++ Seq(sectionRow, badHeaders, validQualifiedDataRow)
      )

      val result = parser.parse(csv)

      result match {
        case Invalid(errors) =>
          errors.exists(e => e.code == "header_mismatch" && e.column.contains("Company UTR")) mustBe true
          errors.forall(_.message == "The selected file must use the template") mustBe true
        case _ =>
          fail("Expected parser to fail when headers are invalid")
      }
    }

    "must return row-level validation errors for invalid values" in {
      val badRow = validQualifiedDataRow
        .updated(0, "Test Plc!")
        .updated(1, "123")
        .updated(2, "AB12")
        .updated(3, "PB")
        .updated(4, "Active1")
        .updated(5, "31/04/2025")
        .updated(6, "x")
        .updated(7, "YES")
        .updated(17, "")

      val csv = toCsv(
        descriptiveRows ++ Seq(sectionRow, UploadTemplateCsvParser.ExpectedHeaders, badRow)
      )

      val result = parser.parse(csv)

      result match {
        case Invalid(errors) =>
          errors.map(_.code) must contain allOf (
            "invalid_company_name",
            "invalid_company_utr",
            "invalid_company_crn",
            "invalid_company_type",
            "invalid_company_status",
            "invalid_tax_regime_value",
            "invalid_financial_year_end_date",
            "missing_qualified_reason"
          )
        case _ =>
          fail("Expected parser to fail when row values are invalid")
      }
    }

    "must return certificate type error when certificate type is blank and no tax regimes are marked" in {
      val badRow = validUnqualifiedDataRow.updated(16, "")

      val csv = toCsv(
        descriptiveRows ++ Seq(sectionRow, UploadTemplateCsvParser.ExpectedHeaders, badRow)
      )

      val result = parser.parse(csv)

      result match {
        case Invalid(errors) =>
          errors.map(_.code) must contain("invalid_certificate_type")
        case _ =>
          fail("Expected parser to fail when certificate type is blank without any tax regimes")
      }
    }

    "must return certificate type error when certificate type is unqualified but tax regime is selected" in {
      val badRow = validQualifiedDataRow.updated(16, "unqualified")

      val csv = toCsv(
        descriptiveRows ++ Seq(sectionRow, UploadTemplateCsvParser.ExpectedHeaders, badRow)
      )

      val result = parser.parse(csv)

      result match {
        case Invalid(errors) =>
          errors.map(_.code) must contain("invalid_certificate_type")
        case _ =>
          fail("Expected parser to fail for unqualified certificate with selected tax regime")
      }
    }

    "must return certificate type error when certificate type is qualified but no tax regimes are selected" in {
      val badRow = validUnqualifiedDataRow.updated(16, "qualified")

      val csv = toCsv(
        descriptiveRows ++ Seq(sectionRow, UploadTemplateCsvParser.ExpectedHeaders, badRow)
      )

      val result = parser.parse(csv)

      result match {
        case Invalid(errors) =>
          errors.map(_.code) must contain("invalid_certificate_type")
        case _ =>
          fail("Expected parser to fail for qualified certificate without selected tax regimes")
      }
    }

    "must handle BOM, commas and new lines in quoted fields" in {
      val quotedRow = validQualifiedDataRow.updated(17, "Line1, with comma\nLine2")

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
            certificateType = CertificateType.Qualified,
            additionalInformation = Some("Line1, with comma\nLine2")
          )
        )
      )
    }
  }
}
