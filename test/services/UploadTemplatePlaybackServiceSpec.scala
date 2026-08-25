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
import pages.*
import pages.notification.*
import services.UploadTemplatePlaybackService.Playback

import java.time.LocalDate

class UploadTemplatePlaybackServiceSpec extends SpecBase {

  private val service = new UploadTemplatePlaybackService()

  private val tableData = UploadTemplateTableData(
    rows = Seq(
      ParsedSubmissionRow(
        notification = NotificationFields(
          companyName = "Acme Plc",
          companyUtr = CompanyUtr("0123456789"),
          companyCrn = Some(CompanyCrn("12345678")),
          companyType = CompanyType.PLC,
          companyStatus = CompanyStatus.Active,
          financialYearEndDate = LocalDate.of(2025, 12, 31)
        ),
        certificate = Some(
          CertificateFields(
            corporationTax = true,
            valueAddedTax = false,
            paye = false,
            insurancePremiumTax = false,
            stampDutyLandTax = false,
            stampDutyReserveTax = false,
            petroleumRevenueTax = false,
            customsDuties = false,
            exciseDuties = false,
            bankLevy = false,
            certificateType = Some(CertificateType.Qualified),
            qualificationStatement = Some("Example")
          )
        )
      )
    ),
    errors = Seq.empty
  )

  "UploadTemplatePlaybackService.getPlayback" - {

    "must return table data and the one SAO name when there was one SAO" in {
      val answers = emptyUserAnswers
        .set(UploadTemplateTablePage, tableData)
        .success
        .value
        .set(NotificationMoreThanOneSaoPage, false)
        .success
        .value
        .set(NotificationSingleSaoOfficerNamePage, "Jane Smith")
        .success
        .value

      service.getPlayback(answers) mustBe Some(Playback(tableData, "Jane Smith"))
    }

    "must return table data and the last SAO name when there was more than one SAO" in {
      val answers = emptyUserAnswers
        .set(UploadTemplateTablePage, tableData)
        .success
        .value
        .set(NotificationMoreThanOneSaoPage, true)
        .success
        .value
        .set(NotificationSingleSaoOfficerNamePage, "Ignored Name")
        .success
        .value
        .set(NotificationMultiSaoLastOfficerNamePage, "John Smith")
        .success
        .value

      service.getPlayback(answers) mustBe Some(Playback(tableData, "John Smith"))
    }

    "must return None when table data is missing" in {
      val answers = emptyUserAnswers
        .set(NotificationMoreThanOneSaoPage, false)
        .success
        .value
        .set(NotificationSingleSaoOfficerNamePage, "Jane Smith")
        .success
        .value

      service.getPlayback(answers) mustBe None
    }

    "must return None when the one SAO name is missing" in {
      val answers = emptyUserAnswers
        .set(UploadTemplateTablePage, tableData)
        .success
        .value
        .set(NotificationMoreThanOneSaoPage, false)
        .success
        .value

      service.getPlayback(answers) mustBe None
    }

    "must return None when the last SAO name is missing" in {
      val answers = emptyUserAnswers
        .set(UploadTemplateTablePage, tableData)
        .success
        .value
        .set(NotificationMoreThanOneSaoPage, true)
        .success
        .value

      service.getPlayback(answers) mustBe None
    }

    "must return None when the more than one SAO answer is missing" in {
      val answers = emptyUserAnswers
        .set(UploadTemplateTablePage, tableData)
        .success
        .value
        .set(NotificationSingleSaoOfficerNamePage, "Jane Smith")
        .success
        .value

      service.getPlayback(answers) mustBe None
    }
  }
}
