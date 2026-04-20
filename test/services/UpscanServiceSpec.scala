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
import connectors.UpscanDownloadConnector
import models.*
import models.upload.*
import org.mockito.ArgumentMatchers.{eq as meq, *}
import org.mockito.Mockito.*
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.mockito.MockitoSugar.mock
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import pages.NotificationUploadStatePage
import play.api.Application
import play.api.http.Status.{BAD_REQUEST, OK}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import services.UpscanService.State
import services.UpscanServiceSpec.*
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.Future
import scala.util.Random

import java.time.LocalDate

class UpscanServiceSpec extends SpecBase with GuiceOneAppPerSuite with BeforeAndAfterEach {

  val mockUpscanDownloadConnector: UpscanDownloadConnector = mock[UpscanDownloadConnector]
  val mockUploadTemplateCsvParser: UploadTemplateCsvParser = mock[UploadTemplateCsvParser]
  given HeaderCarrier                                      = HeaderCarrier()

  override def beforeEach(): Unit = {
    reset(mockUpscanDownloadConnector)
    reset(mockUploadTemplateCsvParser)
  }

  def SUT: UpscanService = app.injector.instanceOf[UpscanService]

  override lazy val app: Application = GuiceApplicationBuilder()
    .overrides(
      bind[UpscanDownloadConnector].toInstance(mockUpscanDownloadConnector),
      bind[UploadTemplateCsvParser].toInstance(mockUploadTemplateCsvParser)
    )
    .build()

  "UpscanService.fileUploadState" - {
    "must return State.NoReference when no reference is received" in {
      val result = SUT.fileUploadState(emptyUserAnswers, None).futureValue

      result mustBe State.NoReference

      verify(mockUpscanDownloadConnector, times(0)).download(any())(using any())
    }

    "must return State.NoReference when no upload state is stored in user answers" in {
      val result = SUT.fileUploadState(emptyUserAnswers, Some(testFileReference)).futureValue

      result mustBe State.NoReference

      verify(mockUpscanDownloadConnector, times(0)).download(any())(using any())
    }

    "must return State.NoReference when the provided reference does not match the stored upload reference" in {
      val userAnswers = userAnswersWithUploadStatus(UploadStatus.InProgress)
      val result      = SUT.fileUploadState(userAnswers, Some("different-reference")).futureValue

      result mustBe State.NoReference

      verify(mockUpscanDownloadConnector, times(0)).download(any())(using any())
    }

    "must return State.WaitingForUpscan when the file upload is in progress" in {
      val result =
        SUT.fileUploadState(userAnswersWithUploadStatus(UploadStatus.InProgress), Some(testFileReference)).futureValue

      result mustBe State.WaitingForUpscan

      verify(mockUpscanDownloadConnector, times(0)).download(any())(using any())
    }

    "must return State.UploadToUpscanFailed when the file upload has failed" in {
      val result =
        SUT.fileUploadState(userAnswersWithUploadStatus(UploadStatus.Failed), Some(testFileReference)).futureValue

      result mustBe State.UploadToUpscanFailed

      verify(mockUpscanDownloadConnector, times(0)).download(any())(using any())
    }

    "must return State.Result when the file upload is completed and the file is downloaded from upscan successfully" in {
      val testResponse = HttpResponse(status = OK, body = testFileContent)
      val parsedRows   = Seq(
        ParsedSubmissionRow(
          notification = NotificationFields(
            companyName = "Test Company",
            companyUtr = CompanyUtr("0123456789"),
            companyCrn = Some(CompanyCrn("12345678")),
            companyType = CompanyType.PLC,
            companyStatus = CompanyStatus.Active,
            financialYearEndDate = LocalDate.of(2025, 12, 31)
          ),
          certificate = CertificateFields(
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
            additionalInformation = None
          )
        )
      )
      when(mockUpscanDownloadConnector.download(any())(using any())).thenReturn(
        Future.successful(testResponse)
      )
      when(mockUploadTemplateCsvParser.parse(any())).thenReturn(TemplateParseResult.Valid(parsedRows))

      val result = SUT
        .fileUploadState(
          userAnswersWithUploadStatus(
            UploadStatus.UploadedSuccessfully(
              name = "",
              mimeType = "",
              downloadUrl = testDownloadUrl,
              size = None
            )
          ),
          Some(testFileReference)
        )
        .futureValue

      result mustBe State.Result(testFileReference, parsedRows)

      verify(mockUpscanDownloadConnector, times(1)).download(meq(testDownloadUrl))(using any())
      verify(mockUploadTemplateCsvParser, times(1)).parse(meq(testFileContent))
    }

    "must return State.ValidationFailed when the downloaded CSV is invalid" in {
      val testResponse = HttpResponse(status = OK, body = testFileContent)
      val parseErrors  = Seq(
        TemplateParseError(
          line = 8,
          column = Some("Company UTR"),
          code = "header_mismatch",
          message = "invalid header"
        )
      )

      when(mockUpscanDownloadConnector.download(any())(using any())).thenReturn(
        Future.successful(testResponse)
      )
      when(mockUploadTemplateCsvParser.parse(any())).thenReturn(TemplateParseResult.Invalid(parseErrors))

      val result = SUT
        .fileUploadState(
          userAnswersWithUploadStatus(
            UploadStatus.UploadedSuccessfully(
              name = "",
              mimeType = "",
              downloadUrl = testDownloadUrl,
              size = None
            )
          ),
          Some(testFileReference)
        )
        .futureValue

      result mustBe State.ValidationFailed(parseErrors)

      verify(mockUpscanDownloadConnector, times(1)).download(meq(testDownloadUrl))(using any())
      verify(mockUploadTemplateCsvParser, times(1)).parse(meq(testFileContent))
    }

    "must return State.DownloadFromUpscanFailed when the file upload is completed but the file download from upscan fails" in {
      val testResponse = HttpResponse(status = BAD_REQUEST, body = testFileContent)
      when(mockUpscanDownloadConnector.download(any())(using any())).thenReturn(
        Future.successful(testResponse)
      )

      val result = SUT
        .fileUploadState(
          userAnswersWithUploadStatus(
            UploadStatus.UploadedSuccessfully(
              name = "",
              mimeType = "",
              downloadUrl = testDownloadUrl,
              size = None
            )
          ),
          Some(testFileReference)
        )
        .futureValue

      result mustBe State.DownloadFromUpscanFailed(testResponse)

      verify(mockUpscanDownloadConnector, times(1)).download(meq(testDownloadUrl))(using any())
    }
  }
}

object UpscanServiceSpec {
  val testDownloadUrl: String   = "/test/url"
  val testFileContent: String   = Random.nextString(10)
  val testFileReference: String = Random.nextString(10)

  def userAnswersWithUploadStatus(status: UploadStatus): UserAnswers =
    UserAnswers("id")
      .set(NotificationUploadStatePage, NotificationUploadState(testFileReference, status))
      .get
}
