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
import services.csvparser.UploadTemplateCsvParser
import services.csvparser.UploadTemplateCsvSchema.TemplateError.InvalidTemplateError
import services.csvparser.UploadTemplateCsvSchema.{Column, TemplateError}
import utils.TestDataGenerator.generateAlphanumeric

import scala.io.Source

import java.time.LocalDate

class UploadTemplateCsvParserSpec extends SpecBase with GuiceOneAppPerSuite {

  private val parser = app.injector.instanceOf[UploadTemplateCsvParser]

  private def parsedRow(
      companyName: String,
      companyType: CompanyType,
      companyStatus: CompanyStatus,
      companyCrn: Option[CompanyCrn] = Some(CompanyCrn("12345678")),
      corporationTax: Boolean = true,
      certificateType: CertificateType,
      additionalInformation: Option[String]
  ) =
    ParsedSubmissionRow(
      notification = NotificationFields(
        companyName = companyName,
        companyUtr = CompanyUtr("0123456789"),
        companyCrn = companyCrn,
        companyType = companyType,
        companyStatus = companyStatus,
        financialYearEndDate = LocalDate.of(2025, 12, 31)
      ),
      certificate = Some(
        CertificateFields(
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
          qualificationStatement = additionalInformation
        )
      )
    )

  private val descriptiveRows: Seq[Seq[String]] =
    (1 to 10).map(index => Seq(s"Descriptive row $index"))

  private val sectionRows: Seq[Seq[String]] = Seq(
    Seq("", "Notification", "", "", "", "", "Certificate"),
    Seq(
      "",
      "",
      "",
      "",
      "",
      "",
      "Mark tax regimes with an 'x' where the company did not have the appropriate tax accounting arrangements.",
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
      "Explain why the certificate is qualified"
    )
  )

  private val validQualifiedDataRow: Seq[String] = Seq(
    "Test Plc",
    "12345678",
    "0123456789",
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
    "12345678",
    "0123456789",
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
          sectionRows ++
          Seq(UploadTemplateCsvParser.ExpectedHeaders, validQualifiedDataRow)
      )

      val result = parser.parse(csv, notificationOnly = false)

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
          sectionRows ++
          Seq(UploadTemplateCsvParser.ExpectedHeaders, blankDataRow, validQualifiedDataRow)
      )

      val result = parser.parse(csv, notificationOnly = false)

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
          sectionRows ++
          Seq(UploadTemplateCsvParser.ExpectedHeaders, validUnqualifiedDataRow)
      )

      val result = parser.parse(csv, notificationOnly = false)

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

    "must parse a valid row when the company CRN is blank" in {
      val rowWithBlankCrn = validUnqualifiedDataRow.updated(1, "")

      val csv = toCsv(
        descriptiveRows ++
          sectionRows ++
          Seq(UploadTemplateCsvParser.ExpectedHeaders, rowWithBlankCrn)
      )

      val result = parser.parse(csv, notificationOnly = false)

      result mustBe Valid(
        Seq(
          parsedRow(
            companyName = "Beta Ltd",
            companyType = CompanyType.LTD,
            companyStatus = CompanyStatus.Dormant,
            companyCrn = None,
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
        descriptiveRows ++ sectionRows ++ Seq(UploadTemplateCsvParser.ExpectedHeaders, autoQualifiedRow)
      )

      val result = parser.parse(csv, notificationOnly = false)

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
            sectionRows(1),
            UploadTemplateCsvParser.ExpectedHeaders,
            validQualifiedDataRow
          )
      )

      val result = parser.parse(csv, notificationOnly = false)

      result match {
        case Invalid(errors) =>
          errors.count(_.error == InvalidTemplateError) mustBe 2
          errors.map(_.line).distinct mustBe Seq(11)
        case _ =>
          fail("Expected parser to fail when section row is invalid")
      }
    }

    "must return errors when a descriptive row is missing" in {
      val csv = toCsv(
        descriptiveRows.take(9) ++ sectionRows ++
          Seq(UploadTemplateCsvParser.ExpectedHeaders, validQualifiedDataRow)
      )

      val result = parser.parse(csv, notificationOnly = false)

      result match {
        case Invalid(errors) =>
          errors.count(_.error == InvalidTemplateError) mustBe 20
          errors.map(_.line).distinct must contain allOf (11, 13)
        case _ =>
          fail("Expected parser to fail when a descriptive row is missing")
      }
    }

    "must return errors when headers do not exactly match" in {
      val badHeaders = UploadTemplateCsvParser.ExpectedHeaders.updated(2, "Company UTR BAD")

      val csv = toCsv(
        descriptiveRows ++ sectionRows ++ Seq(badHeaders, validQualifiedDataRow)
      )

      val result = parser.parse(csv, notificationOnly = false)

      result match {
        case Invalid(errors) =>
          errors.count(_.error == InvalidTemplateError) mustBe 1
        case _ =>
          fail("Expected parser to fail when headers are invalid")
      }
    }

    "must ignore unexpected extra data columns" in {
      val rowWithExtraColumn = validQualifiedDataRow :+ "unexpected"

      val csv = toCsv(
        descriptiveRows ++ sectionRows ++ Seq(UploadTemplateCsvParser.ExpectedHeaders, rowWithExtraColumn)
      )

      val result = parser.parse(csv, notificationOnly = false)

      result must not be an[Invalid]
    }

    "must return an invalid CSV error when CSV content cannot be parsed" in {
      val result = parser.parse("\"unterminated", notificationOnly = false)

      result match {
        case Invalid(errors) =>
          errors.headOption.value.line mustBe 0
          errors.headOption.value.column mustBe None
          errors.headOption.value.error mustBe TemplateError.InvalidTemplateError
        case _ =>
          fail("Expected parser to fail when CSV content cannot be parsed")
      }
    }

    "must return validation errors for invalid values" in {
      val badRow = validQualifiedDataRow
        .updated(0, generateAlphanumeric(161))
        .updated(1, "123")
        .updated(2, "AB12")
        .updated(3, "PB")
        .updated(4, "Active1")
        .updated(5, "31/04/2025")
        .updated(6, "x")
        .updated(7, "YES")
        .updated(17, "")

      val csv = toCsv(
        descriptiveRows ++ sectionRows ++ Seq(UploadTemplateCsvParser.ExpectedHeaders, badRow)
      )

      val result = parser.parse(csv, notificationOnly = false)

      result match {
        case Invalid(errors) =>
          errors.map(_.error) must contain allOf (
            TemplateError.CompanyNameError,
            TemplateError.UtrError,
            TemplateError.CrnError,
            TemplateError.CompanyTypeError,
            TemplateError.CompanyStatusError,
            TemplateError.TaxRegimeError,
            TemplateError.FinancialYearEndDateError,
            TemplateError.QualificationStatementMissingError
          )
        case _ =>
          fail("Expected parser to fail when row values are invalid")
      }
    }

    "must return validation error for qualification reason is too long" in {
      val badRow = validQualifiedDataRow
        .updated(17, generateAlphanumeric(5001))

      val csv = toCsv(
        descriptiveRows ++ sectionRows ++ Seq(UploadTemplateCsvParser.ExpectedHeaders, badRow)
      )

      val result = parser.parse(csv, notificationOnly = false)

      result match {
        case Invalid(errors) =>
          errors.map(_.error) must contain(
            TemplateError.QualificationStatementTooLongError
          )
        case _ =>
          fail("Expected parser to fail when row values are invalid")
      }
    }

    "must return validation error for qualification reason being prohibited for an unqualified entity" in {
      val badRow = validUnqualifiedDataRow
        .updated(17, generateAlphanumeric(1))

      val csv = toCsv(
        descriptiveRows ++ sectionRows ++ Seq(UploadTemplateCsvParser.ExpectedHeaders, badRow)
      )

      val result = parser.parse(csv, notificationOnly = false)

      result match {
        case Invalid(errors) =>
          errors.map(_.error) must contain(
            TemplateError.QualificationStatementProhibitedError
          )
        case _ =>
          fail("Expected parser to fail when row values are invalid")
      }
    }

    "must return certificate type error when certificate type is blank and no tax regimes are marked" in {
      val badRow = validUnqualifiedDataRow.updated(16, "")

      val csv = toCsv(
        descriptiveRows ++ sectionRows ++ Seq(UploadTemplateCsvParser.ExpectedHeaders, badRow)
      )

      val result = parser.parse(csv, notificationOnly = false)

      result match {
        case Invalid(errors) =>
          errors.map(_.error) must contain(TemplateError.CertificateTypeError)
        case _ =>
          fail("Expected parser to fail when certificate type is blank without any tax regimes")
      }
    }

    "must return certificate type error when certificate type is unqualified but tax regime is selected" in {
      val badRow = validQualifiedDataRow.updated(16, "unqualified")

      val csv = toCsv(
        descriptiveRows ++ sectionRows ++ Seq(UploadTemplateCsvParser.ExpectedHeaders, badRow)
      )

      val result = parser.parse(csv, notificationOnly = false)

      result match {
        case Invalid(errors) =>
          errors.map(_.error) must contain(TemplateError.CertificateTypeError)
        case _ =>
          fail("Expected parser to fail for unqualified certificate with selected tax regime")
      }
    }

    "must return certificate type error when certificate type is qualified but no tax regimes are selected" in {
      val badRow = validUnqualifiedDataRow.updated(16, "qualified")

      val csv = toCsv(
        descriptiveRows ++ sectionRows ++ Seq(UploadTemplateCsvParser.ExpectedHeaders, badRow)
      )

      val result = parser.parse(csv, notificationOnly = false)

      result match {
        case Invalid(errors) =>
          errors.map(_.error) must contain(TemplateError.CertificateTypeError)
        case _ =>
          fail("Expected parser to fail for qualified certificate without selected tax regimes")
      }
    }

    "must handle byte order correctly, commas and new lines in quoted fields" in {
      val quotedRow = validQualifiedDataRow.updated(17, "Line1, with comma\nLine2")

      val csv = "\uFEFF" + toCsv(
        descriptiveRows ++ sectionRows ++ Seq(UploadTemplateCsvParser.ExpectedHeaders, quotedRow)
      )

      val result = parser.parse(csv, notificationOnly = false)

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

    "when evaluating the test files" - {
      def readFile(file: String): String = Source.fromResource(file).mkString

      "must pass for" - {
        Map(
          " Z Moderately Complex (4 SAOs - Submission A- AEFG).csv"             -> 4,
          " Z Quite Complex (3 SAOs - Submission A- AEF).csv"                   -> 3,
          "Z Descendants (8 Companies, 2 SAOs - Submission A- ACDE).csv"        -> 4,
          "Z Massive Pass (1000 Companies).csv"                                 -> 1000,
          "Z Moderately Complex (4 SAOs - Submission B- BC).csv"                -> 2,
          "Z Multiple Company Types (PLC + Private).csv"                        -> 2,
          "Z Semi-Public (B and C only - Private companies in mixed group).csv" -> 2,
          "Z Simplex (Single Company).csv"                                      -> 1
        ).foreach { case (file, expectedNumberEntries) =>
          s"$file" in {
            val csv: String =
              readFile(s"templates/testonly/CSV scenarios/pass/$file")

            val result = parser.parse(csv, notificationOnly = false)

            result mustBe an[Valid]
            result.asInstanceOf[Valid].rows.length mustBe expectedNumberEntries
          }
        }

        "'Certificate only Errors.csv' when notificationOnly = true" in {
          val csv: String =
            readFile(s"templates/testonly/CSV scenarios/fail/Certificate only Errors.csv")

          val result = parser.parse(csv, notificationOnly = true)

          result mustBe an[Valid]
          result.asInstanceOf[Valid].rows.length mustBe 3
        }
      }

      "must fail for" - {

        Seq(
          "emptyfile.csv",
          "emptyTemplate.csv"
        ).foreach { file =>
          s"$file with the resultant UploadTemplateTableData.notSaoTemplateOrIsEmpty=true" in {
            val csv: String =
              readFile(s"templates/testonly/CSV scenarios/fail/$file")

            val result = parser.parse(csv, notificationOnly = false)

            val mappedUploadTemplateTableData = result match {
              case Valid(rows)     => UploadTemplateTableData(rows = rows, errors = Seq.empty)
              case Invalid(errors) => UploadTemplateTableData(rows = Seq.empty, errors = errors)
            }

            mappedUploadTemplateTableData.notSaoTemplateOrIsEmpty mustBe true
          }
        }

        Map(
          "Z Massive Fail (1000 Companies).csv"                              -> 6500,
          "Z Moderately Complex (4 SAOs - Submission A- AEFG) - Failure.csv" -> 4,
          "Certificate only Errors.csv"                                      -> 14
        ).foreach { case (file, expectedErrors) =>
          s"$file with the resultant UploadTemplateTableData.notSaoTemplateOrIsEmpty=false" in {
            val csv: String =
              readFile(s"templates/testonly/CSV scenarios/fail/$file")

            val result = parser.parse(csv, notificationOnly = false)

            result mustBe an[Invalid]
            val invalid = result.asInstanceOf[Invalid]
            invalid.errors.length mustBe expectedErrors

            UploadTemplateTableData(rows = Seq.empty, errors = invalid.errors).notSaoTemplateOrIsEmpty mustBe false
          }
        }
      }
    }

  }
}
