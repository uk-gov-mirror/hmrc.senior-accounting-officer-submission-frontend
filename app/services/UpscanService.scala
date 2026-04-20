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

import connectors.UpscanDownloadConnector
import models.UploadStatus.*
import models.upload.{ParsedSubmissionRow, TemplateParseError, TemplateParseResult}
import models.{NotificationUploadState, UserAnswers}
import pages.NotificationUploadStatePage
import play.api.http.Status.OK
import services.UpscanService.*
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

import javax.inject.Inject

class UpscanService @Inject() (
    downloadConnector: UpscanDownloadConnector,
    uploadTemplateCsvParser: UploadTemplateCsvParser
)(using ExecutionContext) {

  def fileUploadState(userAnswers: UserAnswers, reference: Option[String])(using hc: HeaderCarrier): Future[State] =
    userAnswers.get(NotificationUploadStatePage).fold(Future.successful(State.NoReference)) { uploadState =>
      reference match {
        case Some(expectedReference) if uploadState.reference != expectedReference =>
          Future.successful(State.NoReference)
        case _ =>
          fileUploadState(uploadState)
      }
    }

  private def fileUploadState(uploadState: NotificationUploadState)(using hc: HeaderCarrier): Future[State] =
    checkUploadState(uploadState).flatMap {
      _.fold(
        state => Future.successful(state),
        { case InterimResult(reference, downloadUrl) =>
          downloadConnector.download(downloadUrl).map {
            case HttpResponse(OK, body, _) =>
              uploadTemplateCsvParser.parse(body) match {
                case TemplateParseResult.Valid(rows) =>
                  State.Result(reference, rows)
                case TemplateParseResult.Invalid(errors) =>
                  State.ValidationFailed(errors)
              }
            case httpResponse =>
              State.DownloadFromUpscanFailed(httpResponse)
          }
        }
      )
    }

  private def checkUploadState(uploadState: NotificationUploadState): Future[Either[State, InterimResult]] =
    Future.successful {
      uploadState.status match {
        case InProgress =>
          Left(State.WaitingForUpscan)
        case UploadedSuccessfully(_, _, downloadUrl, _) =>
          Right(InterimResult(uploadState.reference, downloadUrl))
        case Failed =>
          Left(State.UploadToUpscanFailed)
      }
    }
}

object UpscanService {

  private final case class InterimResult(reference: String, fileContent: String)

  enum State {
    case NoReference                                               extends State
    case WaitingForUpscan                                          extends State
    case UploadToUpscanFailed                                      extends State
    case DownloadFromUpscanFailed(response: HttpResponse)          extends State
    case ValidationFailed(errors: Seq[TemplateParseError])         extends State
    case Result(reference: String, rows: Seq[ParsedSubmissionRow]) extends State
  }
}
