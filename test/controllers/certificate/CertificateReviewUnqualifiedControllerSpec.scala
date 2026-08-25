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
import models.*
import models.certificate.CertificateTaskListStage
import models.upload.*
import navigation.{CertificateNavigator, FakeCertificateNavigator}
import pages.certificate.CertificateUploadTemplateTablePage
import play.api.inject.bind
import play.api.mvc.Call
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import views.html.certificate.CertificateReviewUnqualifiedView

import scala.util.Random

import java.time.LocalDate

import CertificateReviewUnqualifiedControllerSpec.*

class CertificateReviewUnqualifiedControllerSpec extends SpecBase {

  def onwardRoute: Call = Call("GET", "/foo")

  "CertificateReviewUnqualified Controller" - {

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
        val request = FakeRequest(GET, certificateRoutes.CertificateReviewUnqualifiedController.onPageLoad().url)

        val result = route(application, request).value

        val view = application.injector.instanceOf[CertificateReviewUnqualifiedView]

        status(result) mustEqual OK
        contentAsString(result) mustEqual view(
          saoName = "Firstname Lastname",
          unqualifiedCompanies = testUnqualifiedCompanies,
          companyCount = testTemplateData.rows.size
        )(using request, messages(application)).toString
      }
    }

    "must redirect to the next page for a POST" in {

      val application =
        applicationBuilder(userAnswers = Some(userAnswersWithCertificateSaoDetails))
          .overrides(
            bind[CertificateNavigator].toInstance(new FakeCertificateNavigator(onwardRoute))
          )
          .build()

      running(application) {
        val request =
          FakeRequest(POST, certificateRoutes.CertificateReviewUnqualifiedController.onSubmit().url)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual onwardRoute.url
      }
    }

    "must redirect to task list on provide sao details stage when user answers is empty for a GET" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()
      running(application) {
        val request = FakeRequest(GET, certificateRoutes.CertificateReviewUnqualifiedController.onPageLoad().url)

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
        val request = FakeRequest(POST, certificateRoutes.CertificateReviewUnqualifiedController.onSubmit().url)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual certificateRoutes.CertificateTaskListController
          .onPageLoad(CertificateTaskListStage.ProvideSaoDetailsStage)
          .url
      }
    }
  }
}

object CertificateReviewUnqualifiedControllerSpec {
  private val testDate1 = LocalDate.now()
  private val testDate2 = LocalDate.now().minusDays(1)

  def utr(seed: Int): String = f"${Random(seed).nextLong(10000000000L)}%09d"
  def crn(seed: Int): String = f"${Random(seed).nextLong(100000000L)}%08d"

  private val testTemplateData = UploadTemplateTableData(
    rows = Seq(
      ParsedSubmissionRow(
        notification = NotificationFields(
          companyName = "example company name",
          companyUtr = CompanyUtr(utr(1)),
          companyCrn = Some(CompanyCrn(crn(1))),
          companyType = CompanyType.LTD,
          companyStatus = CompanyStatus.Administration,
          financialYearEndDate = testDate1
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
      ),
      ParsedSubmissionRow(
        notification = NotificationFields(
          companyName = "example company name 2",
          companyUtr = CompanyUtr(utr(2)),
          companyCrn = Some(CompanyCrn(crn(2))),
          companyType = CompanyType.LTD,
          companyStatus = CompanyStatus.Dormant,
          financialYearEndDate = testDate2
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
            qualificationStatement = Some("example additional information ")
          )
        )
      ),
      ParsedSubmissionRow(
        notification = NotificationFields(
          companyName = "example company name 3",
          companyUtr = CompanyUtr(utr(3)),
          companyCrn = Some(CompanyCrn(crn(3))),
          companyType = CompanyType.LTD,
          companyStatus = CompanyStatus.Active,
          financialYearEndDate = LocalDate.now()
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
            qualificationStatement = Some("example additional information 3")
          )
        )
      )
    ),
    errors = Seq.empty
  )

  val testUnqualifiedCompanies: Seq[UnqualifiedCompany] = Seq(
    UnqualifiedCompany(
      name = "example company name",
      utr = utr(1),
      crn = Some(crn(1)),
      companyType = CompanyType.LTD,
      companyStatus = CompanyStatus.Administration,
      financialYearEndDate = testDate1
    ),
    UnqualifiedCompany(
      name = "example company name 2",
      utr = utr(2),
      crn = Some(crn(2)),
      companyType = CompanyType.LTD,
      companyStatus = CompanyStatus.Dormant,
      financialYearEndDate = testDate2
    )
  )

}
