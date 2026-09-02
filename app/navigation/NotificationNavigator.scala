/*
 * Copyright 2025 HM Revenue & Customs
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

package navigation

import controllers.notification.routes as notificationRoutes
import controllers.routes
import models.*
import models.upload.UploadTemplateTableData
import pages.*
import pages.notification.*
import play.api.mvc.Call

import javax.inject.{Inject, Singleton}

@Singleton
class NotificationNavigator @Inject() () extends Navigator {

  override protected val normalRoutes: Page => UserAnswers => Call = {
    case NotificationAdditionalInformationPage =>
      _ => notificationRoutes.ConfirmYourNotificationController.onPageLoad()
    case ConfirmYourNotificationPage =>
      _ => notificationRoutes.NotificationCheckYourAnswersController.onPageLoad()
    case NotificationConfirmationPage =>
      _ => notificationRoutes.NotificationTaskListController.onComplete()
    case NotificationMoreThanOneSaoPage =>
      userAnswers =>
        userAnswers.get(NotificationMoreThanOneSaoPage) match {
          case Some(true)  => notificationRoutes.NotificationMultiSaoLastOfficerNameController.onPageLoad(NormalMode)
          case Some(false) => notificationRoutes.NotificationSingleSaoOfficerNameController.onPageLoad(NormalMode)
          case _           => ???
        }
    case NotificationSingleSaoOfficerNamePage =>
      _ => notificationRoutes.NotificationTaskListController.onPageLoad()
    case NotificationMultiSaoLastOfficerNamePage =>
      _ => notificationRoutes.NotificationMultiSaoLastOfficerStartDateController.onPageLoad(NormalMode)
    case NotificationMultiSaoLastOfficerStartDatePage =>
      _ => notificationRoutes.NotificationMultiSaoPreviousOfficerNameController.onPageLoad(NormalMode)
    case NotificationMultiSaoPreviousOfficerNamePage(saoIndex) =>
      _ => notificationRoutes.NotificationMultiSaoPreviousOfficerStartDateController.onPageLoad(NormalMode, saoIndex)
    case NotificationMultiSaoPreviousOfficerStartDatePage(saoIndex) =>
      _ => notificationRoutes.NotificationMultiSaoPreviousOfficerEndDateController.onPageLoad(NormalMode, saoIndex)
    case NotificationMultiSaoPreviousOfficerEndDatePage(saoIndex) =>
      _ => notificationRoutes.NotificationMultiSaoAreAllAddedController.onPageLoad(NormalMode, saoIndex)
    case NotificationMultiSaoAreAllAddedPage(saoIndex) =>
      userAnswers =>
        userAnswers.get(NotificationMultiSaoAreAllAddedPage(saoIndex)) match {
          case Some(true)  => notificationRoutes.NotificationTaskListController.onPageLoad()
          case Some(false) =>
            notificationRoutes.NotificationMultiSaoPreviousOfficerNameController.onPageLoad(NormalMode, saoIndex + 1)
          case _ => ???
        }
    case UploadTemplateTablePage =>
      userAnswers =>
        userAnswers
          .get(UploadTemplateTablePage)
          .fold(routes.JourneyRecoveryController.onPageLoad()) {
            case UploadTemplateTableData(_, errors) if errors.nonEmpty =>
              notificationRoutes.NotificationUploadFormController.onPageLoad()
            case _ => notificationRoutes.NotificationTaskListController.onPageLoad()
          }
    case _ => _ => ???
  }

  override protected val checkRouteMap: Page => UserAnswers => Call = {
    case NotificationAdditionalInformationPage =>
      _ => notificationRoutes.NotificationCheckYourAnswersController.onPageLoad()
    case NotificationSingleSaoOfficerNamePage =>
      _ => notificationRoutes.NotificationCheckYourAnswersController.onPageLoad()
    case _ => _ => ???
  }

}
