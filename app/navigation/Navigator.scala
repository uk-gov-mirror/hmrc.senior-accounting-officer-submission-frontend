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

import controllers.routes
import models.*
import models.upload.UploadTemplateDebugData
import pages.*
import play.api.mvc.Call

import javax.inject.{Inject, Singleton}

@Singleton
class Navigator @Inject() () {

  private val normalRoutes: Page => UserAnswers => Call = {
    case NotificationGuidancePage =>
      _ => routes.NotificationAdditionalInformationController.onPageLoad(NormalMode)
    case NotificationAdditionalInformationPage =>
      _ => routes.NotificationCheckYourAnswersController.onPageLoad()
    case NotificationCheckYourAnswersPage =>
      _ => routes.SubmitNotificationController.onPageLoad()
    case SubmitNotificationPage =>
      _ => routes.NotificationConfirmationController.onPageLoad()
    case SubmitCertificateStartPage =>
      _ => routes.IsThisTheSaoOnCertificateController.onPageLoad(NormalMode)
    case IsThisTheSaoOnCertificatePage =>
      userAnswers =>
        userAnswers.get(IsThisTheSaoOnCertificatePage) match {
          case Some(true)  => routes.SaoEmailController.onPageLoad(NormalMode)
          case Some(false) => routes.SaoNameController.onPageLoad(NormalMode)
          case _           => ???
        }
    case SaoNamePage =>
      _ => routes.SaoEmailController.onPageLoad(NormalMode)
    case SaoEmailPage =>
      _ => routes.SaoEmailCommunicationChoiceController.onPageLoad(NormalMode)
    case SaoEmailCommunicationChoicePage =>
      _ => routes.CertificateCheckYourAnswersController.onPageLoad()
    case CertificateCheckYourAnswersPage =>
      _ => routes.WhoSubmitsCertificateController.onPageLoad(NormalMode)
    case WhoSubmitsCertificatePage =>
      _ => routes.QualifiedCompaniesController.onPageLoad()
    case QualifiedCompaniesPage =>
      _ => routes.UnqualifiedCompaniesController.onPageLoad()
    case UnqualifiedCompaniesPage =>
      _ => routes.CertificateSubmissionDeclarationController.onPageLoad(NormalMode)
    case CertificateSubmissionDeclarationPage =>
      _ => routes.CertificateConfirmationController.onPageLoad()
    case NotificationConfirmationPage =>
      _ => routes.SubmitCertificateStartController.onPageLoad()
    case UploadTemplateDebugPage =>
      userAnswers =>
        userAnswers
          .get(UploadTemplateDebugPage)
          .map {
            case UploadTemplateDebugData(_, errors) if errors.nonEmpty =>
              routes.NotificationUploadFormController.onPageLoad()
            case _ =>
              routes.SubmitNotificationStartController.onPageLoad()
          }
          .getOrElse(routes.JourneyRecoveryController.onPageLoad())
    case _ =>
      _ => ???
  }

  private val checkRouteMap: Page => UserAnswers => Call = {
    case NotificationAdditionalInformationPage =>
      _ => routes.NotificationCheckYourAnswersController.onPageLoad()
    case SaoNamePage =>
      _ => routes.CertificateCheckYourAnswersController.onPageLoad()
    case SaoEmailPage =>
      _ => routes.CertificateCheckYourAnswersController.onPageLoad()
    case SaoEmailCommunicationChoicePage =>
      _ => routes.CertificateCheckYourAnswersController.onPageLoad()
    case IsThisTheSaoOnCertificatePage =>
      userAnswers =>
        userAnswers.get(IsThisTheSaoOnCertificatePage) match {
          case Some(true)  => routes.CertificateCheckYourAnswersController.onPageLoad()
          case Some(false) => routes.SaoNameController.onPageLoad(CheckMode)
          case _           => ???
        }
    case _ => _ => ???
  }

  def nextPage(page: Page, mode: Mode, userAnswers: UserAnswers): Call = mode match {
    case NormalMode =>
      normalRoutes(page)(userAnswers)
    case CheckMode =>
      checkRouteMap(page)(userAnswers)
  }
}
