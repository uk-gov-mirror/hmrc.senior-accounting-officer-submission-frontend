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

package models.upload

import base.SpecBase
import models.QualifiedCompany
import models.UnqualifiedCompany
import models.upload.ParsedSubmissionRowSpec.*
import play.api.libs.json.{JsError, JsString, Json}
import utils.TestDataGenerator.*

import java.time.LocalDate

class ParsedSubmissionRowSpec extends SpecBase {
  import NotificationFields.given

  "ParsedSubmissionRow json formats" - {

    "must round-trip a fully-populated parsed submission row" in {
      val row = ParsedSubmissionRow(
        notification = NotificationFields(
          companyName = "Acme Ltd",
          companyUtr = CompanyUtr(generateUtr),
          companyCrn = Some(CompanyCrn(generateCrn)),
          companyType = CompanyType.LTD,
          companyStatus = CompanyStatus.Active,
          financialYearEndDate = LocalDate.of(2025, 12, 31)
        ),
        certificate = Some(
          CertificateFields(
            corporationTax = true,
            valueAddedTax = false,
            paye = true,
            insurancePremiumTax = false,
            stampDutyLandTax = false,
            stampDutyReserveTax = false,
            petroleumRevenueTax = false,
            customsDuties = false,
            exciseDuties = false,
            bankLevy = false,
            certificateType = Some(CertificateType.Qualified),
            qualificationStatement = Some("Additional context")
          )
        )
      )

      Json.toJson(row).validate[ParsedSubmissionRow].get mustBe row
    }

    "must read local date values in dd/MM/yyyy format" in {
      JsString("31/12/2025").validate[LocalDate].get mustBe LocalDate.of(2025, 12, 31)
    }

    "must reject invalid local date values" in {
      JsString("31/02/2025").validate[LocalDate] mustBe JsError("Invalid date value: 31/02/2025")
    }
  }

  "value parsers" - {

    "must parse and trim CompanyUtr and CompanyCrn values" in {
      val utr = generateUtr
      val crn = generateCrn
      CompanyUtr.fromString(s" $utr ").value mustBe CompanyUtr(utr)
      CompanyCrn.fromString(s" $crn ").value mustBe CompanyCrn(crn)
    }

    "must capitalise the CompanyCrn values" in {
      CompanyCrn.fromString(" qq123456 ").value mustBe CompanyCrn("QQ123456")
    }

    "must reject invalid CompanyUtr and CompanyCrn values" in {
      CompanyUtr.fromString("123") mustBe None
      CompanyCrn.fromString("12") mustBe None
      CompanyCrn.fromString("abc12345") mustBe None
    }

    "must parse enum values case-insensitively" in {
      CompanyType.fromString("plc").value mustBe CompanyType.PLC
      CompanyStatus.fromString("dormant").value mustBe CompanyStatus.Dormant
      CertificateType.fromString("unQUALified").value mustBe CertificateType.Unqualified
    }

    "must reject unknown enum values" in {
      JsString("OTHER").validate[CompanyType] mustBe JsError("Unknown enum value: OTHER")
      JsString("OTHER").validate[CompanyStatus] mustBe JsError("Unknown enum value: OTHER")
      JsString("OTHER").validate[CertificateType] mustBe JsError("Unknown enum value: OTHER")
    }
  }

  "toQualifiedCompany extension method must" - {
    "map a qualified company from ParsedSubmissionRow to Some(QualifiedCompany)" in {
      val testDate = LocalDate.now()
      val result   = ParsedSubmissionRow(
        notification = NotificationFields(
          companyName = testCompanyName,
          companyUtr = CompanyUtr(testCompanyUtr),
          companyCrn = Some(CompanyCrn(testCompanyCrn)),
          companyType = CompanyType.LTD,
          companyStatus = CompanyStatus.Dormant,
          financialYearEndDate = testDate
        ),
        certificate = Some(
          CertificateFields(
            corporationTax = true,
            valueAddedTax = false,
            paye = true,
            insurancePremiumTax = false,
            stampDutyLandTax = true,
            stampDutyReserveTax = false,
            petroleumRevenueTax = true,
            customsDuties = false,
            exciseDuties = true,
            bankLevy = false,
            certificateType = Some(CertificateType.Qualified),
            qualificationStatement = Some(testAdditionalInformation)
          )
        )
      ).toQualifiedCompany

      val expected = Some(
        QualifiedCompany(
          name = testCompanyName,
          utr = testCompanyUtr,
          crn = Some(testCompanyCrn),
          companyType = "LTD",
          status = "Dormant",
          financialYearEndDate = testDate,
          corporationTax = true,
          valueAddedTax = false,
          paye = true,
          insurancePremiumTax = false,
          stampDutyLandTax = true,
          stampDutyReserveTax = false,
          petroleumRevenueTax = true,
          customsDuties = false,
          exciseDuties = true,
          bankLevy = false,
          additionalInformation = testAdditionalInformation
        )
      )

      result mustBe expected
    }

    "map an unqualified company from ParsedSubmissionRow to None" in {
      val result = ParsedSubmissionRow(
        notification = NotificationFields(
          companyName = testCompanyName,
          companyUtr = CompanyUtr(testCompanyUtr),
          companyCrn = Some(CompanyCrn(testCompanyCrn)),
          companyType = CompanyType.LTD,
          companyStatus = CompanyStatus.Dormant,
          financialYearEndDate = LocalDate.now()
        ),
        certificate = Some(
          CertificateFields(
            corporationTax = false,
            valueAddedTax = false,
            paye = false,
            insurancePremiumTax = false,
            stampDutyLandTax = false,
            stampDutyReserveTax = false,
            petroleumRevenueTax = false,
            customsDuties = false,
            exciseDuties = false,
            bankLevy = false,
            certificateType = Some(CertificateType.Unqualified),
            qualificationStatement = Some(testAdditionalInformation)
          )
        )
      ).toQualifiedCompany

      result mustBe None

    }

  }

  "toUnqualifiedCompany extension method must" - {
    "map an unqualified company from ParsedSubmissionRow to Some(UnqualifiedCompany)" in {
      val testDate = LocalDate.now()
      val result   = ParsedSubmissionRow(
        notification = NotificationFields(
          companyName = "example company name",
          companyUtr = CompanyUtr("example company utr"),
          companyCrn = Some(CompanyCrn("example company crn")),
          companyType = CompanyType.LTD,
          companyStatus = CompanyStatus.Dormant,
          financialYearEndDate = testDate
        ),
        certificate = Some(
          CertificateFields(
            corporationTax = false,
            valueAddedTax = false,
            paye = false,
            insurancePremiumTax = false,
            stampDutyLandTax = false,
            stampDutyReserveTax = false,
            petroleumRevenueTax = false,
            customsDuties = false,
            exciseDuties = false,
            bankLevy = false,
            certificateType = Some(CertificateType.Unqualified),
            qualificationStatement = Some("example additional information")
          )
        )
      ).toUnqualifiedCompany

      val expected = Some(
        UnqualifiedCompany(
          name = "example company name",
          utr = "example company utr",
          crn = Some("example company crn"),
          companyType = CompanyType.LTD,
          companyStatus = CompanyStatus.Dormant,
          financialYearEndDate = testDate
        )
      )

      result mustBe expected
    }

    "map a qualified company from ParsedSubmissionRow to None" in {
      val result = ParsedSubmissionRow(
        notification = NotificationFields(
          companyName = "example company name",
          companyUtr = CompanyUtr("example company utr"),
          companyCrn = Some(CompanyCrn("example company crn")),
          companyType = CompanyType.LTD,
          companyStatus = CompanyStatus.Dormant,
          financialYearEndDate = LocalDate.now()
        ),
        certificate = Some(
          CertificateFields(
            corporationTax = true,
            valueAddedTax = false,
            paye = true,
            insurancePremiumTax = false,
            stampDutyLandTax = true,
            stampDutyReserveTax = false,
            petroleumRevenueTax = true,
            customsDuties = false,
            exciseDuties = true,
            bankLevy = false,
            certificateType = Some(CertificateType.Qualified),
            qualificationStatement = Some("example additional information")
          )
        )
      ).toUnqualifiedCompany

      result mustBe None
    }
  }
}

object ParsedSubmissionRowSpec {
  val testCompanyName           = "example company name"
  val testCompanyUtr            = "example company utr"
  val testCompanyCrn            = "example company crn"
  val testAdditionalInformation = "example additional information"
}
