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
import models.requests.DataRequest
import models.upload.UploadTemplateTableData
import pages.UploadTemplateTablePage
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
      upscanService.fileUploadState(request.userAnswers, key, messagesApi.preferred(request)).flatMap {
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
          saveTableDataAndRedirect(
            UploadTemplateTableData(rows = Seq.empty, errors = errors),
            routes.UploadTemplateTableErrorController.onPageLoad()
          )
        case State.Result(reference, rows) =>
          Logger(getClass).info(s"Uploaded template parsed successfully, reference: $reference, rows: ${rows.size}")
          saveTableDataAndRedirect(
            UploadTemplateTableData(rows = rows, errors = Seq.empty),
            routes.UploadTemplateTableController.onPageLoad()
          )
      }
    }

  private def saveTableDataAndRedirect(
      tableData: UploadTemplateTableData,
      redirectTo: Call
  )(using request: DataRequest[AnyContent]): Future[Result] =
    for {
      updatedAnswers <- Future.fromTry(request.userAnswers.set(UploadTemplateTablePage, tableData))
      _              <- sessionRepository.set(updatedAnswers)
    } yield Redirect(redirectTo)

}
