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
import connectors.ProtectedServiceConnector
import models.UserAnswers
import models.notification.*
import models.upload.*
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar.mock
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import pages.notification.*
import play.api.Application
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.Helpers.*
import repositories.SessionRepository
import services.NotificationSubmitService.toNotification
import services.NotificationSubmitServiceSpec.*
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import utils.TestDataGenerator

import scala.concurrent.Future

import java.time.LocalDate

class NotificationSubmitServiceSpec extends SpecBase with GuiceOneAppPerSuite {

  "NotificationSubmitService.submit" - {

    given HeaderCarrier = HeaderCarrier()

    val userAnswers = emptyUserAnswers
      .set(NotificationMoreThanOneSaoPage, false)
      .success
      .value
      .set(NotificationSingleSaoOfficerNamePage, "Jackson Brown")
      .success
      .value
      .set(UploadTemplateTablePage, UploadTemplateTableData(rows = Seq.empty, errors = Seq.empty))
      .success
      .value

    "must return notification response on success" in {
      val application = configureApplication(
        HttpResponse(OK, Json.obj("notificationRef" -> exampleNotificationReference).toString),
        true
      )

      running(application) {
        val SUT    = application.injector.instanceOf[NotificationSubmitService]
        val result = SUT.submit(userAnswers).futureValue
        result mustBe Right(exampleNotificationReference)
      }
    }

    "must return error on http failure" in {
      val application = configureApplication(
        HttpResponse(INTERNAL_SERVER_ERROR),
        true
      )

      running(application) {
        val SUT    = application.injector.instanceOf[NotificationSubmitService]
        val result = SUT.submit(userAnswers).futureValue
        result.isLeft mustBe true
        result.left.map(error => error.message mustBe expectedHttpFailureMessage)
      }
    }

    def configureApplication(mockConnectorResponse: HttpResponse, mockRepositoryResponse: Boolean): Application = {
      val mockConnector = mock[ProtectedServiceConnector]

      when(mockConnector.postNotification(any())(using any[HeaderCarrier]())) thenReturn Future.successful(
        mockConnectorResponse
      )

      val mockRepository = mock[SessionRepository]

      when(mockRepository.set(any())).thenReturn(
        Future.successful(
          mockRepositoryResponse
        )
      )

      applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[ProtectedServiceConnector].toInstance(mockConnector),
          bind[SessionRepository].toInstance(mockRepository)
        )
        .build()
    }
  }

  "NotificationSubmitService.toNotification" - {
    def buildUserAnswers(moreThanOneSao: Boolean): UserAnswers = {
      emptyUserAnswers
        .set(NotificationAdditionalInformationPage, Some(exampleAdditionalInformation))
        .success
        .value
        .set(NotificationMoreThanOneSaoPage, moreThanOneSao)
        .success
        .value
        .set(NotificationSingleSaoOfficerNamePage, exampleSao1Name)
        .success
        .value
        .set(NotificationMultiSaoLastOfficerNamePage, exampleSao2Name)
        .success
        .value
        .set(NotificationMultiSaoLastOfficerStartDatePage, exampleSao2StartDate)
        .success
        .value
        .set(NotificationMultiSaoPreviousOfficerNamePage(0), exampleSao3Name)
        .success
        .value
        .set(NotificationMultiSaoPreviousOfficerStartDatePage(0), exampleSao3StartDate)
        .success
        .value
        .set(NotificationMultiSaoPreviousOfficerEndDatePage(0), exampleSao3EndDate)
        .success
        .value
        .set(NotificationMultiSaoPreviousOfficerNamePage(1), exampleSao4Name)
        .success
        .value
        .set(NotificationMultiSaoPreviousOfficerStartDatePage(1), exampleSao4StartDate)
        .success
        .value
        .set(NotificationMultiSaoPreviousOfficerEndDatePage(1), exampleSao4EndDate)
        .success
        .value
        .set(UploadTemplateTablePage, exampleTableData)
        .success
        .value
    }

    "User has provided one SAO" in {
      val userAnswers = buildUserAnswers(false)

      val expected = NotificationRequest(
        saos = List(Sao(name = exampleSao1Name, fromDate = None, toDate = None)),
        companies = List(
          Company(
            name = exampleCompanyName,
            accPeriodEnd = exampleAccPeriodEnd.toString,
            crn = Some(exampleCrn),
            utr = exampleUtr,
            status = CompanyStatus.Active,
            `type` = CompanyType.LTD
          )
        ),
        remarks = Some(exampleAdditionalInformation)
      )

      val result = userAnswers.toNotification

      result mustBe expected
    }

    "User has provided multiple SAOs" in {
      val userAnswers = buildUserAnswers(true)

      val expected = NotificationRequest(
        saos = List(
          Sao(
            name = exampleSao2Name,
            fromDate = Some(exampleSao2StartDate.toString),
            toDate = None
          ),
          Sao(
            name = exampleSao3Name,
            fromDate = Some(exampleSao3StartDate.toString),
            toDate = Some(exampleSao3EndDate.toString)
          ),
          Sao(
            name = exampleSao4Name,
            fromDate = Some(exampleSao4StartDate.toString),
            toDate = Some(exampleSao4EndDate.toString)
          )
        ),
        companies = List(
          Company(
            name = exampleCompanyName,
            accPeriodEnd = exampleAccPeriodEnd.toString,
            crn = Some(exampleCrn),
            utr = exampleUtr,
            status = CompanyStatus.Active,
            `type` = CompanyType.LTD
          )
        ),
        remarks = Some(exampleAdditionalInformation)
      )

      val result = userAnswers.toNotification

      result mustBe expected
    }
  }
}

object NotificationSubmitServiceSpec {
  val exampleNotificationReference       = "appleBananaCitrue"
  val expectedHttpFailureMessage: String = s"Notification submit HTTP call failed with code ${INTERNAL_SERVER_ERROR}"

  val exampleAdditionalInformation    = "example additional information"
  val exampleSao1Name                 = "Firstname Lastname I"
  val exampleSao2Name                 = "Firstname Lastname II"
  val exampleSao2StartDate: LocalDate = LocalDate.of(2000, 1, 2)
  val exampleSao2EndDate: LocalDate   = LocalDate.of(2000, 12, 2)
  val exampleSao3Name                 = "Firstname Lastname III"
  val exampleSao3StartDate: LocalDate = LocalDate.of(2000, 1, 3)
  val exampleSao3EndDate: LocalDate   = LocalDate.of(2000, 12, 3)
  val exampleSao4Name                 = "Firstname Lastname IV"
  val exampleSao4StartDate: LocalDate = LocalDate.of(2000, 1, 4)
  val exampleSao4EndDate: LocalDate   = LocalDate.of(2000, 12, 4)

  val exampleCompanyName             = "example company name"
  val exampleAccPeriodEnd: LocalDate = LocalDate.of(2001, 1, 1)

  lazy val exampleCrn = TestDataGenerator.generateCrn
  lazy val exampleUtr = TestDataGenerator.generateUtr

  val exampleTableData: UploadTemplateTableData = UploadTemplateTableData(
    rows = Seq(
      ParsedSubmissionRow(
        notification = NotificationFields(
          companyName = exampleCompanyName,
          companyUtr = CompanyUtr.fromString(exampleUtr).get,
          companyCrn = Some(CompanyCrn.fromString(exampleCrn).get),
          companyType = CompanyType.LTD,
          companyStatus = CompanyStatus.Active,
          financialYearEndDate = exampleAccPeriodEnd
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
            certificateType = None,
            qualificationStatement = None
          )
        )
      )
    ),
    errors = Seq.empty
  )
}
