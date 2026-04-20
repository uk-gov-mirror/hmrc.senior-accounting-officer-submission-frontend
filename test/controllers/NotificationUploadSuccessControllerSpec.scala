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

package controllers

import base.SpecBase
import controllers.NotificationUploadSuccessControllerSpec.*
import models.*
import models.upload.ParsedSubmissionRow
import models.upload.TemplateParseError
import org.mockito.ArgumentMatchers.{eq as meq, *}
import org.mockito.Mockito.*
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.mockito.MockitoSugar.mock
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import repositories.SessionRepository
import services.UpscanService
import services.UpscanService.State
import uk.gov.hmrc.http.HttpResponse
import views.html.NotificationUploadSuccessView

import scala.concurrent.Future
import scala.util.Random

class NotificationUploadSuccessControllerSpec extends SpecBase with BeforeAndAfterEach {
  val mockUpscanService: UpscanService         = mock[UpscanService]
  val mockSessionRepository: SessionRepository = mock[SessionRepository]

  override def beforeEach(): Unit = {
    reset(mockUpscanService)
    reset(mockSessionRepository)
    when(mockSessionRepository.set(any())).thenReturn(Future.successful(true))
  }

  override def applicationBuilder(userAnswers: Option[UserAnswers] = None): GuiceApplicationBuilder = super
    .applicationBuilder(userAnswers = Some(emptyUserAnswers))
    .overrides(
      bind[UpscanService].toInstance(mockUpscanService),
      bind[SessionRepository].toInstance(mockSessionRepository)
    )

  "NotificationUploadSuccess Controller" - {

    "when UpscanService returns State.NoReference" - {
      "must return Redirect to Journey recovery" in {
        when(mockUpscanService.fileUploadState(any[UserAnswers], any[Option[String]])(using any())).thenReturn(
          Future.successful(State.NoReference)
        )

        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

        running(application) {
          val request =
            FakeRequest(GET, routes.NotificationUploadSuccessController.onPageLoad(Some(testFileReference)).url)

          val result = route(application, request).value

          application.injector.instanceOf[NotificationUploadSuccessView]

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).get mustEqual routes.JourneyRecoveryController.onPageLoad().url

          verify(mockUpscanService, times(1)).fileUploadState(any[UserAnswers], meq(Some(testFileReference)))(using
            any()
          )
        }
      }

    }

    "when no key is provided" - {
      "must return Redirect to Journey recovery" in {
        when(mockUpscanService.fileUploadState(any[UserAnswers], meq(None))(using any())).thenReturn(
          Future.successful(State.NoReference)
        )

        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

        running(application) {
          val request =
            FakeRequest(GET, routes.NotificationUploadSuccessController.onPageLoad(None).url)

          val result = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).get mustEqual routes.JourneyRecoveryController.onPageLoad().url

          verify(mockUpscanService, times(1)).fileUploadState(any[UserAnswers], meq(None))(using any())
        }
      }
    }

    "when UpscanService returns State.WaitingForUpscan" - {
      "must return OK and the correct view for a GET" in {
        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

        when(mockUpscanService.fileUploadState(any[UserAnswers], any[Option[String]])(using any())).thenReturn(
          Future.successful(State.WaitingForUpscan)
        )

        running(application) {
          val request =
            FakeRequest(GET, routes.NotificationUploadSuccessController.onPageLoad(Some(testFileReference)).url)

          val result = route(application, request).value

          val view = application.injector.instanceOf[NotificationUploadSuccessView]

          status(result) mustEqual OK
          contentAsString(result) mustEqual view()(using request, messages(application)).toString

          verify(mockUpscanService, times(1)).fileUploadState(any[UserAnswers], meq(Some(testFileReference)))(using
            any()
          )
        }
      }
    }

    "when UpscanService returns State.UploadToUpscanFailed" - {
      "must redirect to Notification upload error page" in {
        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

        when(mockUpscanService.fileUploadState(any[UserAnswers], any[Option[String]])(using any())).thenReturn(
          Future.successful(State.UploadToUpscanFailed)
        )

        running(application) {
          val request =
            FakeRequest(GET, routes.NotificationUploadSuccessController.onPageLoad(Some(testFileReference)).url)
          val result = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).value mustBe routes.NotificationUploadErrorController.onPageLoad().url

          verify(mockUpscanService, times(1)).fileUploadState(any[UserAnswers], meq(Some(testFileReference)))(using
            any()
          )
        }
      }
    }

    "when UpscanService returns State.DownloadFromUpscanFailed" - {
      "must redirect to Notification upload error page" in {
        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

        when(mockUpscanService.fileUploadState(any[UserAnswers], any[Option[String]])(using any())).thenReturn(
          Future.successful(State.DownloadFromUpscanFailed(HttpResponse(status = BAD_REQUEST, body = testFileContent)))
        )

        running(application) {
          val request =
            FakeRequest(GET, routes.NotificationUploadSuccessController.onPageLoad(Some(testFileReference)).url)

          val result = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).value mustBe routes.NotificationUploadErrorController.onPageLoad().url

          verify(mockUpscanService, times(1)).fileUploadState(any[UserAnswers], meq(Some(testFileReference)))(using
            any()
          )
        }
      }
    }

    "when UpscanService returns State.ValidationFailed" - {
      "must redirect to upload debug page" in {
        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

        when(mockUpscanService.fileUploadState(any[UserAnswers], any[Option[String]])(using any())).thenReturn(
          Future.successful(
            State.ValidationFailed(
              Seq(
                TemplateParseError(
                  line = 8,
                  column = Some("Company UTR"),
                  code = "header_mismatch",
                  message = "invalid header"
                )
              )
            )
          )
        )

        running(application) {
          val request =
            FakeRequest(GET, routes.NotificationUploadSuccessController.onPageLoad(Some(testFileReference)).url)

          val result = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).value mustBe routes.UploadTemplateDebugController.onPageLoad().url

          verify(mockUpscanService, times(1)).fileUploadState(any[UserAnswers], meq(Some(testFileReference)))(using
            any()
          )
          verify(mockSessionRepository, times(1)).set(any())
        }
      }
    }

    "when UpscanService returns State.Result" - {
      "must redirect to upload debug page" in {
        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

        when(mockUpscanService.fileUploadState(any[UserAnswers], any[Option[String]])(using any())).thenReturn(
          Future.successful(State.Result(testFileReference, parsedRows))
        )

        running(application) {
          val request =
            FakeRequest(GET, routes.NotificationUploadSuccessController.onPageLoad(Some(testFileReference)).url)

          val result = route(application, request).value

          application.injector.instanceOf[NotificationUploadSuccessView]

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).get mustBe routes.UploadTemplateDebugController.onPageLoad().url

          verify(mockUpscanService, times(1)).fileUploadState(any[UserAnswers], meq(Some(testFileReference)))(using
            any()
          )
          verify(mockSessionRepository, times(1)).set(any())
        }
      }
    }
  }
}

object NotificationUploadSuccessControllerSpec {
  val parsedRows: Seq[ParsedSubmissionRow] = Seq.empty
  val testDownloadUrl: String              = "/test/url"
  val testFileContent: String              = Random.nextString(10)
  val testFileReference: String            = Random.nextString(10)
}
