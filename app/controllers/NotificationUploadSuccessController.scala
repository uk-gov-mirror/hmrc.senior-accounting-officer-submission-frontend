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

import controllers.actions.*
import models.upload.UploadTemplateDebugData
import pages.UploadTemplateDebugPage
import play.api.Logger
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.*
import repositories.SessionRepository
import services.UpscanService
import services.UpscanService.State
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.NotificationUploadSuccessView

import scala.concurrent.{ExecutionContext, Future}

import javax.inject.Inject

class NotificationUploadSuccessController @Inject() (
    override val messagesApi: MessagesApi,
    identify: IdentifierAction,
    getData: DataRetrievalAction,
    requireData: DataRequiredAction,
    val controllerComponents: MessagesControllerComponents,
    sessionRepository: SessionRepository,
    upscanService: UpscanService,
    view: NotificationUploadSuccessView
)(using ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  def onPageLoad(key: Option[String]): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      upscanService.fileUploadState(request.userAnswers, key).flatMap {
        case State.NoReference =>
          Future.successful(Redirect(routes.JourneyRecoveryController.onPageLoad()))
        case State.WaitingForUpscan =>
          Future.successful(Ok(view()))
        case State.UploadToUpscanFailed =>
          Future.successful(Redirect(routes.NotificationUploadErrorController.onPageLoad()))
        case State.DownloadFromUpscanFailed(response) =>
          Logger(getClass).warn(s"Failed to download uploaded template from Upscan: ${response.status}")
          Future.successful(Redirect(routes.NotificationUploadErrorController.onPageLoad()))
        case State.ValidationFailed(errors) =>
          Logger(getClass).warn(s"Uploaded template failed validation with ${errors.size} error(s)")
          val debugData = UploadTemplateDebugData(rows = Seq.empty, errors = errors)
          for {
            updatedAnswers <- Future.fromTry(request.userAnswers.set(UploadTemplateDebugPage, debugData))
            _              <- sessionRepository.set(updatedAnswers)
          } yield Redirect(routes.UploadTemplateDebugController.onPageLoad())
        case State.Result(reference, rows) =>
          Logger(getClass).info(s"Uploaded template parsed successfully, reference: $reference, rows: ${rows.size}")
          val debugData = UploadTemplateDebugData(rows = rows, errors = Seq.empty)
          for {
            updatedAnswers <- Future.fromTry(request.userAnswers.set(UploadTemplateDebugPage, debugData))
            _              <- sessionRepository.set(updatedAnswers)
          } yield Redirect(routes.UploadTemplateDebugController.onPageLoad())
      }
    }

}
