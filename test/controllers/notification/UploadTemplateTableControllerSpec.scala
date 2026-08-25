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

package controllers.notification

import base.SpecBase
import controllers.notification.routes as notificationRoutes
import controllers.routes
import models.upload.*
import navigation.{FakeNotificationNavigator, NotificationNavigator}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{never, verify, when}
import org.scalatestplus.mockito.MockitoSugar
import pages.*
import pages.notification.*
import play.api.http.HeaderNames
import play.api.inject.bind
import play.api.mvc.Call
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import repositories.SessionRepository
import services.csvparser.UploadTemplateCsvSchema.{Column, TemplateError}
import views.html.notification.{UploadTemplateTableErrorView, UploadTemplateTableView}

import scala.concurrent.Future

import java.time.LocalDate

class UploadTemplateTableControllerSpec extends SpecBase with MockitoSugar {

  def onwardRoute: Call = Call("GET", "/foo")

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

  private val saoName          = "Jane Smith"
  private val populatedAnswers = emptyUserAnswers
    .set(UploadTemplateTablePage, tableData)
    .success
    .value
    .set(NotificationMoreThanOneSaoPage, false)
    .success
    .value
    .set(NotificationSingleSaoOfficerNamePage, saoName)
    .success
    .value

  private val errorTableData = UploadTemplateTableData(
    rows = Seq.empty,
    errors = Seq(TemplateParseError(9, Some(Column.Utr), TemplateError.UtrError))
  )

  private val populatedErrorAnswers = completedSaoDetailsAnswers
    .set(UploadTemplateTablePage, errorTableData)
    .success
    .value

  "UploadTemplateTable Controller" - {

    "must return OK and the correct view for a GET" in {
      val application = applicationBuilder(userAnswers = Some(populatedAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, notificationRoutes.UploadTemplateTableController.onPageLoad().url)

        val result = route(application, request).value

        val view = application.injector.instanceOf[UploadTemplateTableView]

        status(result) mustEqual OK
        contentAsString(result) mustEqual view(tableData, saoName)(using request, messages(application)).toString
      }
    }

    "must redirect to the next page for a POST" in {
      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())).thenReturn(Future.successful(true))

      val application = applicationBuilder(userAnswers = Some(populatedAnswers))
        .overrides(
          bind[NotificationNavigator].toInstance(new FakeNotificationNavigator(onwardRoute)),
          bind[SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, notificationRoutes.UploadTemplateTableController.onSubmit().url)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        header(HeaderNames.LOCATION, result) mustEqual Some(onwardRoute.url)
        verify(mockSessionRepository).set(populatedAnswers.set(UploadTemplateReviewPage, true).get)
      }
    }

    "must return OK and the correct error view for a GET when the table data has errors" in {
      val application = applicationBuilder(userAnswers = Some(populatedErrorAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, notificationRoutes.UploadTemplateTableController.onPageLoad().url)

        val result = route(application, request).value

        val view = application.injector.instanceOf[UploadTemplateTableErrorView]

        status(result) mustEqual OK
        contentAsString(result) mustEqual view(errorTableData)(using request, messages(application)).toString
      }
    }

    "must redirect to upload form for a POST when the table data has errors" in {
      val mockSessionRepository = mock[SessionRepository]

      val application = applicationBuilder(userAnswers = Some(populatedErrorAnswers))
        .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
        .build()

      running(application) {
        val request = FakeRequest(POST, notificationRoutes.UploadTemplateTableController.onSubmit().url)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual notificationRoutes.NotificationUploadFormController.onPageLoad().url
        verify(mockSessionRepository, never()).set(any())
      }
    }

    "must return OK and the correct view for a GET when there was more than one SAO" in {
      val lastSaoName = "John Smith"
      val answers     = completedMultipleSaoDetailsAnswers
        .set(UploadTemplateTablePage, tableData)
        .success
        .value
        .set(NotificationMultiSaoLastOfficerNamePage, lastSaoName)
        .success
        .value

      val application = applicationBuilder(userAnswers = Some(answers)).build()

      running(application) {
        val request = FakeRequest(GET, notificationRoutes.UploadTemplateTableController.onPageLoad().url)

        val result = route(application, request).value

        val view = application.injector.instanceOf[UploadTemplateTableView]

        status(result) mustEqual OK
        contentAsString(result) mustEqual view(tableData, lastSaoName)(using request, messages(application)).toString
      }
    }

    "must redirect to Journey Recovery for GET when table data is missing" in {
      val application = applicationBuilder(userAnswers = Some(completedSaoDetailsAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, notificationRoutes.UploadTemplateTableController.onPageLoad().url)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to the task list for GET when SAO name is missing" in {
      val answers     = emptyUserAnswers.set(UploadTemplateTablePage, tableData).success.value
      val application = applicationBuilder(userAnswers = Some(answers)).build()

      running(application) {
        val request = FakeRequest(GET, notificationRoutes.UploadTemplateTableController.onPageLoad().url)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual notificationRoutes.NotificationTaskListController.onPageLoad().url
      }
    }

    "must redirect to Journey Recovery for POST when debug data is missing" in {
      val application = applicationBuilder(userAnswers = Some(completedSaoDetailsAnswers)).build()

      running(application) {
        val request = FakeRequest(POST, notificationRoutes.UploadTemplateTableController.onSubmit().url)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.JourneyRecoveryController.onPageLoad().url
      }
    }
  }
}
