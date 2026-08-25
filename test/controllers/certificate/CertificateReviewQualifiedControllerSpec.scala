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

package controllers.certificate

import base.SpecBase
import controllers.certificate.routes as certificateRoutes
import controllers.routes
import models.QualifiedCompany
import models.certificate.CertificateTaskListStage
import models.upload.*
import navigation.{CertificateNavigator, FakeCertificateNavigator}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{never, verify, when}
import org.scalatestplus.mockito.MockitoSugar
import pages.certificate.CertificateUploadTemplateTablePage
import play.api.inject.bind
import play.api.mvc.Call
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import repositories.SessionRepository
import services.csvparser.UploadTemplateCsvSchema.{Column, TemplateError}
import views.html.certificate.{CertificateReviewQualifiedView, CertificateUploadTemplateTableErrorView}

import scala.concurrent.Future
import scala.util.Random

import java.time.LocalDate

import CertificateReviewQualifiedControllerSpec.*

class CertificateReviewQualifiedControllerSpec extends SpecBase with MockitoSugar {

  def onwardRoute: Call = Call("GET", "/foo")

  "CertificateReviewQualified Controller" - {

    "must return OK and the correct view for a GET" in {

      val application = applicationBuilder(userAnswers =
        Some(
          userAnswersWithCertificateSaoDetails
            .set(CertificateUploadTemplateTablePage, testTemplateData)
            .success
            .value
        )
      ).build()

      running(application) {
        val request = FakeRequest(GET, certificateRoutes.CertificateReviewQualifiedController.onPageLoad().url)

        val result = route(application, request).value

        val view = application.injector.instanceOf[CertificateReviewQualifiedView]

        status(result) mustEqual OK
        contentAsString(result) mustEqual view(
          saoName = "Firstname Lastname",
          companyCount = testTemplateData.rows.size,
          qualifiedCompanies = testQualifiedCompanies
        )(using
          request,
          messages(application)
        ).toString
      }
    }

    "must redirect to the next page for a POST" in {
      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())).thenReturn(Future.successful(true))

      val application =
        applicationBuilder(userAnswers =
          Some(
            userAnswersWithCertificateSaoDetails
              .set(CertificateUploadTemplateTablePage, testTemplateData)
              .success
              .value
          )
        )
          .overrides(
            bind[CertificateNavigator].toInstance(new FakeCertificateNavigator(onwardRoute)),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {
        val request =
          FakeRequest(POST, certificateRoutes.CertificateReviewQualifiedController.onSubmit().url)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual onwardRoute.url
        verify(mockSessionRepository).set(any())
      }
    }

    "must return OK and the correct error view for a GET when the table data has errors" in {
      val application = applicationBuilder(userAnswers =
        Some(
          userAnswersWithCertificateSaoDetails
            .set(CertificateUploadTemplateTablePage, errorTemplateData)
            .success
            .value
        )
      ).build()

      running(application) {
        val request = FakeRequest(GET, certificateRoutes.CertificateReviewQualifiedController.onPageLoad().url)

        val result = route(application, request).value

        val view = application.injector.instanceOf[CertificateUploadTemplateTableErrorView]

        status(result) mustEqual OK
        contentAsString(result) mustEqual view(errorTemplateData)(using request, messages(application)).toString
      }
    }

    "must redirect to upload form for a POST when the table data has errors" in {
      val mockSessionRepository = mock[SessionRepository]

      val application = applicationBuilder(userAnswers =
        Some(
          userAnswersWithCertificateSaoDetails
            .set(CertificateUploadTemplateTablePage, errorTemplateData)
            .success
            .value
        )
      ).overrides(bind[SessionRepository].toInstance(mockSessionRepository)).build()

      running(application) {
        val request = FakeRequest(POST, certificateRoutes.CertificateReviewQualifiedController.onSubmit().url)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual certificateRoutes.CertificateUploadFormController.onPageLoad().url
        verify(mockSessionRepository, never()).set(any())
      }
    }

    "must redirect to Journey Recovery for GET when table data is missing" in {
      val application = applicationBuilder(userAnswers = Some(userAnswersWithCertificateSaoDetails)).build()

      running(application) {
        val request = FakeRequest(GET, certificateRoutes.CertificateReviewQualifiedController.onPageLoad().url)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to Journey Recovery for POST when table data is missing" in {
      val application = applicationBuilder(userAnswers = Some(userAnswersWithCertificateSaoDetails)).build()

      running(application) {
        val request = FakeRequest(POST, certificateRoutes.CertificateReviewQualifiedController.onSubmit().url)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to task list on provide sao details stage when user answers is empty for a GET" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()
      running(application) {
        val request = FakeRequest(GET, certificateRoutes.CertificateReviewQualifiedController.onPageLoad().url)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual certificateRoutes.CertificateTaskListController
          .onPageLoad(CertificateTaskListStage.ProvideSaoDetailsStage)
          .url
      }
    }

    "must redirect to task list on provide sao details stage when user answers is empty for a POST" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()
      running(application) {
        val request = FakeRequest(POST, certificateRoutes.CertificateReviewQualifiedController.onSubmit().url)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual certificateRoutes.CertificateTaskListController
          .onPageLoad(CertificateTaskListStage.ProvideSaoDetailsStage)
          .url
      }
    }
  }
}

object CertificateReviewQualifiedControllerSpec {

  def utr(seed: Int): String = f"${Random(seed).nextLong(10000000000L)}%09d"

  def crn(seed: Int): String = f"${Random(seed).nextLong(100000000L)}%08d"

  private val testDate1 = LocalDate.now()
  private val testDate2 = LocalDate.now().minusDays(1)

  val testTemplateData: UploadTemplateTableData = UploadTemplateTableData(
    rows = Seq(
      ParsedSubmissionRow(
        notification = NotificationFields(
          companyName = "example company name",
          companyUtr = CompanyUtr(utr(1)),
          companyCrn = Some(CompanyCrn(crn(1))),
          companyType = CompanyType.LTD,
          companyStatus = CompanyStatus.Active,
          financialYearEndDate = testDate1
        ),
        certificate = Some(
          CertificateFields(
            corporationTax = false,
            valueAddedTax = true,
            paye = false,
            insurancePremiumTax = true,
            stampDutyLandTax = false,
            stampDutyReserveTax = false,
            petroleumRevenueTax = true,
            customsDuties = false,
            exciseDuties = false,
            bankLevy = false,
            certificateType = Some(CertificateType.Qualified),
            qualificationStatement = Some("example additional information")
          )
        )
      ),
      ParsedSubmissionRow(
        notification = NotificationFields(
          companyName = "example company name 2",
          companyUtr = CompanyUtr(utr(2)),
          companyCrn = None,
          companyType = CompanyType.PLC,
          companyStatus = CompanyStatus.Dormant,
          financialYearEndDate = testDate2
        ),
        certificate = Some(
          CertificateFields(
            corporationTax = false,
            valueAddedTax = true,
            paye = false,
            insurancePremiumTax = true,
            stampDutyLandTax = false,
            stampDutyReserveTax = true,
            petroleumRevenueTax = false,
            customsDuties = false,
            exciseDuties = true,
            bankLevy = false,
            certificateType = Some(CertificateType.Qualified),
            qualificationStatement = Some("example additional information 2")
          )
        )
      ),
      ParsedSubmissionRow(
        notification = NotificationFields(
          companyName = "example company name 3",
          companyUtr = CompanyUtr(utr(3)),
          companyCrn = Some(CompanyCrn(crn(3))),
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
            qualificationStatement = None
          )
        )
      )
    ),
    errors = Seq.empty
  )

  val errorTemplateData: UploadTemplateTableData = UploadTemplateTableData(
    rows = Seq.empty,
    errors = Seq(TemplateParseError(9, Some(Column.Utr), TemplateError.UtrError))
  )

  val testQualifiedCompanies: Seq[QualifiedCompany] = Seq(
    QualifiedCompany(
      name = "example company name",
      utr = utr(1),
      crn = Some(crn(1)),
      companyType = "LTD",
      status = "Active",
      financialYearEndDate = testDate1,
      corporationTax = false,
      valueAddedTax = true,
      paye = false,
      insurancePremiumTax = true,
      stampDutyLandTax = false,
      stampDutyReserveTax = false,
      petroleumRevenueTax = true,
      customsDuties = false,
      exciseDuties = false,
      bankLevy = false,
      additionalInformation = "example additional information"
    ),
    QualifiedCompany(
      name = "example company name 2",
      utr = utr(2),
      crn = None,
      companyType = "PLC",
      status = "Dormant",
      financialYearEndDate = testDate2,
      corporationTax = false,
      valueAddedTax = true,
      paye = false,
      insurancePremiumTax = true,
      stampDutyLandTax = false,
      stampDutyReserveTax = true,
      petroleumRevenueTax = false,
      customsDuties = false,
      exciseDuties = true,
      bankLevy = false,
      additionalInformation = "example additional information 2"
    )
  )

}
