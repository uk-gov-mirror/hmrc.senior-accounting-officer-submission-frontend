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
import models.upscan.{FileUploadState, UploadJourney, UploadStatus}
import org.mockito.ArgumentMatchers.{eq as meq, *}
import org.mockito.Mockito.*
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.mockito.MockitoSugar.mock
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.http.Status.{BAD_REQUEST, OK}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import services.UpscanService.*
import services.UpscanServiceSpec.*
import services.csvparser.UploadTemplateCsvParser
import services.csvparser.UploadTemplateCsvSchema.{Column, TemplateError}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import utils.TestDataGenerator.*

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

  "UpscanService.fileUploadState(UploadJourney.Notification)" - {

    "must return State.ValidationFailed when the downloaded CSV is not empty" in {
      val testResponse = HttpResponse(status = OK, body = testFileContent)

      val rows = Seq(
        ParsedSubmissionRow(
          notification = NotificationFields(
            companyName = "Acme Plc",
            companyUtr = CompanyUtr(generateUtr),
            companyCrn = Some(CompanyCrn(generateCrn)),
            companyType = CompanyType.PLC,
            companyStatus = CompanyStatus.Active,
            financialYearEndDate = LocalDate.of(2025, 12, 31)
          ),
          certificate = None
        )
      )
      val nonEmptyTemplate = TemplateParseResult.Valid(rows = rows)

      when(mockUpscanDownloadConnector.download(any())(using any())).thenReturn(
        Future.successful(testResponse)
      )
      when(mockUploadTemplateCsvParser.parse(any(), any()))
        .thenReturn(nonEmptyTemplate)

      val result = SUT
        .fileUploadState(
          UploadJourney.Notification,
          userAnswersWithUploadStatus(
            UploadJourney.Notification,
            UploadStatus.UploadedSuccessfully(
              name = "submission.csv",
              mimeType = "",
              downloadUrl = testDownloadUrl,
              size = None
            )
          ),
          Some(testFileReference)
        )
        .futureValue

      result mustBe State.Result(testFileReference, rows)

      verify(mockUpscanDownloadConnector, times(1)).download(meq(testDownloadUrl))(using any())
      verify(mockUploadTemplateCsvParser, times(1)).parse(meq(testFileContent), meq(true))
    }

    "must return State.ValidationFailed when the downloaded CSV is empty" in {
      val testResponse = HttpResponse(status = OK, body = testFileContent)

      val emptyTemplate = TemplateParseResult.Valid(rows = Seq.empty)

      when(mockUpscanDownloadConnector.download(any())(using any())).thenReturn(
        Future.successful(testResponse)
      )
      when(mockUploadTemplateCsvParser.parse(any(), any()))
        .thenReturn(emptyTemplate)

      val result = SUT
        .fileUploadState(
          UploadJourney.Notification,
          userAnswersWithUploadStatus(
            UploadJourney.Notification,
            UploadStatus.UploadedSuccessfully(
              name = "submission.csv",
              mimeType = "",
              downloadUrl = testDownloadUrl,
              size = None
            )
          ),
          Some(testFileReference)
        )
        .futureValue

      result mustBe State.ValidationFailed(Seq.empty)

      verify(mockUpscanDownloadConnector, times(1)).download(meq(testDownloadUrl))(using any())
      verify(mockUploadTemplateCsvParser, times(1)).parse(meq(testFileContent), meq(true))
    }

    "must return State.NoReference when no reference is received" in {
      val result = SUT.fileUploadState(UploadJourney.Notification, emptyUserAnswers, None).futureValue

      result mustBe State.NoReference

      verify(mockUpscanDownloadConnector, times(0)).download(any())(using any())
    }

    "must return State.NoReference when no upload state is stored in user answers" in {
      val result =
        SUT.fileUploadState(UploadJourney.Notification, emptyUserAnswers, Some(testFileReference)).futureValue

      result mustBe State.NoReference

      verify(mockUpscanDownloadConnector, times(0)).download(any())(using any())
    }

    "must return State.NoReference when the provided reference does not match the stored upload reference" in {
      val userAnswers = userAnswersWithUploadStatus(UploadJourney.Notification, UploadStatus.InProgress)
      val result = SUT.fileUploadState(UploadJourney.Notification, userAnswers, Some("different-reference")).futureValue

      result mustBe State.NoReference

      verify(mockUpscanDownloadConnector, times(0)).download(any())(using any())
    }

    "must return State.WaitingForUpscan when the file upload is in progress" in {
      val result =
        SUT
          .fileUploadState(
            UploadJourney.Notification,
            userAnswersWithUploadStatus(UploadJourney.Notification, UploadStatus.InProgress),
            Some(testFileReference)
          )
          .futureValue

      result mustBe State.WaitingForUpscan

      verify(mockUpscanDownloadConnector, times(0)).download(any())(using any())
    }

    "must return State.QuarantinedByUpscan when the file is quarantined" in {
      val result =
        SUT
          .fileUploadState(
            UploadJourney.Notification,
            userAnswersWithUploadStatus(UploadJourney.Notification, UploadStatus.Quarantined),
            Some(testFileReference)
          )
          .futureValue

      result mustBe State.QuarantinedByUpscan

      verify(mockUpscanDownloadConnector, times(0)).download(any())(using any())
    }

    "must return State.RejectedByUpscan when the file is rejected" in {
      val result =
        SUT
          .fileUploadState(
            UploadJourney.Notification,
            userAnswersWithUploadStatus(UploadJourney.Notification, UploadStatus.Rejected),
            Some(testFileReference)
          )
          .futureValue

      result mustBe State.RejectedByUpscan

      verify(mockUpscanDownloadConnector, times(0)).download(any())(using any())
    }

    "must return State.UnknownUpscanError when the file is rejected" in {
      val result =
        SUT
          .fileUploadState(
            UploadJourney.Notification,
            userAnswersWithUploadStatus(UploadJourney.Notification, UploadStatus.UnknownFailure),
            Some(testFileReference)
          )
          .futureValue

      result mustBe State.UnknownUpscanError

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
      )
      when(mockUpscanDownloadConnector.download(any())(using any())).thenReturn(
        Future.successful(testResponse)
      )
      when(mockUploadTemplateCsvParser.parse(any(), any()))
        .thenReturn(TemplateParseResult.Valid(parsedRows))

      val result = SUT
        .fileUploadState(
          UploadJourney.Notification,
          userAnswersWithUploadStatus(
            UploadJourney.Notification,
            UploadStatus.UploadedSuccessfully(
              name = "submission.csv",
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
      verify(mockUploadTemplateCsvParser, times(1)).parse(meq(testFileContent), meq(true))
    }

    "must return State.ValidationFailed when the downloaded CSV is invalid" in {
      val testResponse = HttpResponse(status = OK, body = testFileContent)
      val parseErrors  = Seq(
        TemplateParseError(
          line = 8,
          column = Some(Column.Utr),
          TemplateError.InvalidTemplateError
        )
      )

      when(mockUpscanDownloadConnector.download(any())(using any())).thenReturn(
        Future.successful(testResponse)
      )
      when(mockUploadTemplateCsvParser.parse(any(), any()))
        .thenReturn(TemplateParseResult.Invalid(parseErrors))

      val result = SUT
        .fileUploadState(
          UploadJourney.Notification,
          userAnswersWithUploadStatus(
            UploadJourney.Notification,
            UploadStatus.UploadedSuccessfully(
              name = "submission.csv",
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
      verify(mockUploadTemplateCsvParser, times(1)).parse(meq(testFileContent), meq(true))
    }

    "must return State.DownloadFromUpscanFailed when the file upload is completed but the file download from upscan fails" in {
      val testResponse = HttpResponse(status = BAD_REQUEST, body = testFileContent)
      when(mockUpscanDownloadConnector.download(any())(using any())).thenReturn(
        Future.successful(testResponse)
      )

      val result = SUT
        .fileUploadState(
          UploadJourney.Notification,
          userAnswersWithUploadStatus(
            UploadJourney.Notification,
            UploadStatus.UploadedSuccessfully(
              name = "submission.csv",
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

    "must return State.RejectedByUpscan when the uploaded file is empty" in {
      val result = SUT
        .fileUploadState(
          UploadJourney.Notification,
          userAnswersWithUploadStatus(
            UploadJourney.Notification,
            UploadStatus.UploadedSuccessfully(
              name = "submission.xlsx",
              mimeType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
              downloadUrl = testDownloadUrl,
              size = None
            )
          ),
          Some(testFileReference)
        )
        .futureValue

      result mustBe State.RejectedByUpscan

      verify(mockUpscanDownloadConnector, times(0)).download(any())(using any())
      verify(mockUploadTemplateCsvParser, times(0)).parse(any(), any())
    }
  }

  def userAnswersWithUploadStatus(journey: UploadJourney, status: UploadStatus): UserAnswers =
    emptyUserAnswers
      .set(journey.page, FileUploadState(testFileReference, status))
      .get
}

object UpscanServiceSpec {
  val testDownloadUrl: String   = "/test/url"
  val testFileContent: String   = Random.nextString(10)
  val testFileReference: String = Random.nextString(10)
}
