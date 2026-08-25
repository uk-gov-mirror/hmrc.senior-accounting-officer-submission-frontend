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
import controllers.certificate.CertificateUploadSuccessControllerSpec.*
import controllers.certificate.routes as certificateRoutes
import controllers.routes
import models.*
import models.upload.{ParsedSubmissionRow, TemplateParseError}
import models.upscan.UploadJourney
import org.mockito.ArgumentMatchers.{eq as meq, *}
import org.mockito.Mockito.*
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.mockito.MockitoSugar.mock
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers.{status, *}
import repositories.SessionRepository
import services.UpscanService
import services.UpscanService.*
import services.csvparser.UploadTemplateCsvSchema.{Column, TemplateError}
import uk.gov.hmrc.http.HttpResponse
import views.html.certificate.CertificateUploadSuccessView

import scala.concurrent.Future
import scala.util.Random

class CertificateUploadSuccessControllerSpec extends SpecBase with BeforeAndAfterEach {
  val mockUpscanService: UpscanService         = mock[UpscanService]
  val mockSessionRepository: SessionRepository = mock[SessionRepository]

  override def beforeEach(): Unit = {
    reset(mockUpscanService)
    reset(mockSessionRepository)
    when(mockSessionRepository.set(any())).thenReturn(Future.successful(true))
  }

  override def applicationBuilder(userAnswers: Option[UserAnswers] = None): GuiceApplicationBuilder = super
    .applicationBuilder(userAnswers = userAnswers)
    .overrides(
      bind[UpscanService].toInstance(mockUpscanService),
      bind[SessionRepository].toInstance(mockSessionRepository)
    )

  "CertificateUploadSuccess Controller" - {

    "when UpscanService returns State.NoReference" - {
      "must return Redirect to Journey recovery" in {
        when(
          mockUpscanService.fileUploadState(meq(UploadJourney.Certificate), any[UserAnswers], any[Option[String]])(using
            any()
          )
        ).thenReturn(
          Future.successful(State.NoReference)
        )

        val application = applicationBuilder(userAnswers = Some(completedSaoDetailsAnswers)).build()

        running(application) {
          val request =
            FakeRequest(
              GET,
              certificateRoutes.CertificateUploadSuccessController.onPageLoad(Some(testFileReference)).url
            )

          val result = route(application, request).value

          application.injector.instanceOf[CertificateUploadSuccessView]

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).get mustEqual routes.JourneyRecoveryController.onPageLoad().url

          verify(mockUpscanService, times(1)).fileUploadState(
            meq(UploadJourney.Certificate),
            any[UserAnswers],
            meq(Some(testFileReference))
          )(using any())
        }
      }

    }

    "when no key is provided" - {
      "must return Redirect to Journey recovery" in {
        when(
          mockUpscanService.fileUploadState(meq(UploadJourney.Certificate), any[UserAnswers], meq(None))(using any())
        ).thenReturn(
          Future.successful(State.NoReference)
        )

        val application = applicationBuilder(userAnswers = Some(completedSaoDetailsAnswers)).build()

        running(application) {
          val request =
            FakeRequest(GET, certificateRoutes.CertificateUploadSuccessController.onPageLoad(None).url)

          val result = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).get mustEqual routes.JourneyRecoveryController.onPageLoad().url

          verify(mockUpscanService, times(1)).fileUploadState(
            meq(UploadJourney.Certificate),
            any[UserAnswers],
            meq(None)
          )(using any())
        }
      }
    }

    "when UpscanService returns State.WaitingForUpscan" - {
      "must return OK and the correct view for a GET" in {
        val application = applicationBuilder(userAnswers = Some(completedSaoDetailsAnswers)).build()

        when(
          mockUpscanService.fileUploadState(meq(UploadJourney.Certificate), any[UserAnswers], any[Option[String]])(using
            any()
          )
        ).thenReturn(
          Future.successful(State.WaitingForUpscan)
        )

        running(application) {
          val request =
            FakeRequest(
              GET,
              certificateRoutes.CertificateUploadSuccessController.onPageLoad(Some(testFileReference)).url
            )

          val result = route(application, request).value

          val view = application.injector.instanceOf[CertificateUploadSuccessView]

          status(result) mustEqual OK
          contentAsString(result) mustEqual view()(using request, messages(application)).toString

          verify(mockUpscanService, times(1)).fileUploadState(
            meq(UploadJourney.Certificate),
            any[UserAnswers],
            meq(Some(testFileReference))
          )(using any())
        }
      }
    }

    def testFileUploadStateCausesUploadFormRedirect(inState: State): Unit = {
      val application = applicationBuilder(userAnswers = Some(completedSaoDetailsAnswers)).build()

      when(
        mockUpscanService.fileUploadState(meq(UploadJourney.Certificate), any[UserAnswers], any[Option[String]])(using
          any()
        )
      ).thenReturn(
        Future.successful(inState)
      )

      running(application) {
        val request =
          FakeRequest(
            GET,
            certificateRoutes.CertificateUploadSuccessController.onPageLoad(Some(testFileReference)).url
          )
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).get mustEqual certificateRoutes.CertificateUploadFormController.onPageLoad().url

        verify(mockUpscanService, times(1)).fileUploadState(
          meq(UploadJourney.Certificate),
          any[UserAnswers],
          meq(Some(testFileReference))
        )(using any())
      }
    }

    "when UpscanService returns State.QuarantinedByUpscan" - {
      "must redirect to CertificateUploadFormController" in {
        testFileUploadStateCausesUploadFormRedirect(State.QuarantinedByUpscan)
      }
    }

    "when UpscanService returns State.RejectedByUpscan" - {
      "must redirect to CertificateUploadFormController" in {
        testFileUploadStateCausesUploadFormRedirect(State.RejectedByUpscan)

      }
    }

    "when UpscanService returns State.UnknownUpscanError" - {
      "must redirect to CertificateUploadFormController" in {
        testFileUploadStateCausesUploadFormRedirect(State.UnknownUpscanError)
      }
    }

    "when UpscanService returns State.DownloadFromUpscanFailed" - {
      "must redirect to certificate upload form page" in {
        val application = applicationBuilder(userAnswers = Some(completedSaoDetailsAnswers)).build()

        when(
          mockUpscanService.fileUploadState(meq(UploadJourney.Certificate), any[UserAnswers], any[Option[String]])(using
            any()
          )
        ).thenReturn(
          Future.successful(State.DownloadFromUpscanFailed(HttpResponse(status = BAD_REQUEST, body = testFileContent)))
        )

        running(application) {
          val request =
            FakeRequest(
              GET,
              certificateRoutes.CertificateUploadSuccessController.onPageLoad(Some(testFileReference)).url
            )

          val result = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).value mustBe certificateRoutes.CertificateUploadFormController.onPageLoad().url

          verify(mockUpscanService, times(1)).fileUploadState(
            meq(UploadJourney.Certificate),
            any[UserAnswers],
            meq(Some(testFileReference))
          )(using any())
        }
      }
    }

    "when UpscanService returns State.ValidationFailed" - {
      "must redirect to upload table error page" in {
        val application = applicationBuilder(userAnswers = Some(completedSaoDetailsAnswers)).build()

        when(
          mockUpscanService.fileUploadState(
            meq(UploadJourney.Certificate),
            any[UserAnswers],
            any[Option[String]]
          )(using any())
        ).thenReturn(
          Future.successful(
            State.ValidationFailed(
              Seq(
                TemplateParseError(
                  line = 8,
                  column = Some(Column.Utr),
                  error = TemplateError.UtrError
                )
              )
            )
          )
        )

        running(application) {
          val request =
            FakeRequest(
              GET,
              certificateRoutes.CertificateUploadSuccessController.onPageLoad(Some(testFileReference)).url
            )

          val result = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).value mustBe certificateRoutes.CertificateReviewQualifiedController.onPageLoad().url

          verify(mockUpscanService, times(1)).fileUploadState(
            meq(UploadJourney.Certificate),
            any[UserAnswers],
            meq(Some(testFileReference))
          )(using any())
          verify(mockSessionRepository, times(1)).set(any())
        }
      }
    }

    "when UpscanService returns State.Result" - {
      "must redirect to upload table page" in {
        val application = applicationBuilder(userAnswers = Some(completedSaoDetailsAnswers)).build()

        when(
          mockUpscanService.fileUploadState(meq(UploadJourney.Certificate), any[UserAnswers], any[Option[String]])(using
            any()
          )
        ).thenReturn(
          Future.successful(State.Result(testFileReference, parsedRows))
        )

        running(application) {
          val request =
            FakeRequest(
              GET,
              certificateRoutes.CertificateUploadSuccessController.onPageLoad(Some(testFileReference)).url
            )

          val result = route(application, request).value

          application.injector.instanceOf[CertificateUploadSuccessView]

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).get mustBe certificateRoutes.CertificateReviewQualifiedController.onPageLoad().url

          verify(mockUpscanService, times(1)).fileUploadState(
            meq(UploadJourney.Certificate),
            any[UserAnswers],
            meq(Some(testFileReference))
          )(using any())
          verify(mockSessionRepository, times(1)).set(any())
        }
      }
    }
  }
}

object CertificateUploadSuccessControllerSpec {
  val parsedRows: Seq[ParsedSubmissionRow] = Seq.empty
  val testFileContent: String              = Random.nextString(10)
  val testFileReference: String            = Random.nextString(10)
}
