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

import base.SpecBase
import controllers.routes
import models.upload.UploadTemplateTableData
import models.{CheckMode, NormalMode, UserAnswers}
import pages.*

class NavigatorSpec extends SpecBase {

  val navigator = new Navigator

  "Navigator.nextPage" - {

    "in Normal mode" - {

      "must throw an not-implemented error for an unspecified configuration" in {
        case object UnknownPage extends Page
        intercept[NotImplementedError] {
          navigator.nextPage(UnknownPage, NormalMode, UserAnswers("id"))
        }
      }

      "when on NotificationGuidancePage, must go to notification additional information page" in {
        navigator.nextPage(
          NotificationGuidancePage,
          NormalMode,
          UserAnswers("id")
        ) mustBe routes.NotificationAdditionalInformationController.onPageLoad(NormalMode)
      }

      "when on NotificationAdditionalInformationPage, must go to notification check your answers page" in {
        navigator.nextPage(
          NotificationAdditionalInformationPage,
          NormalMode,
          UserAnswers("id")
        ) mustBe routes.NotificationCheckYourAnswersController.onPageLoad()
      }

      "when on NotificationCheckYourAnswersPage, must go to submit notification page" in {
        navigator.nextPage(
          NotificationCheckYourAnswersPage,
          NormalMode,
          UserAnswers("id")
        ) mustBe routes.SubmitNotificationController.onPageLoad()
      }

      "when on SubmitNotificationPage, must go to notification confirmation page" in {
        navigator.nextPage(
          SubmitNotificationPage,
          NormalMode,
          UserAnswers("id")
        ) mustBe routes.NotificationConfirmationController.onPageLoad()
      }

      "when on SubmitCertificateStartPage, must go to is this SAO on certificate page" in {
        navigator.nextPage(
          SubmitCertificateStartPage,
          NormalMode,
          UserAnswers("id")
        ) mustBe routes.IsThisTheSaoOnCertificateController.onPageLoad(NormalMode)
      }

      "when on IsThisTheSaoOnCertificatePage and the user selected Yes, must go to SAO email page" in {
        navigator.nextPage(
          IsThisTheSaoOnCertificatePage,
          NormalMode,
          UserAnswers("id").set(IsThisTheSaoOnCertificatePage, true).get
        ) mustBe routes.SaoEmailController.onPageLoad(NormalMode)
      }

      "when on IsThisTheSaoOnCertificatePage and the user selected No, must go to SAO name page" in {
        navigator.nextPage(
          IsThisTheSaoOnCertificatePage,
          NormalMode,
          UserAnswers("id").set(IsThisTheSaoOnCertificatePage, false).get
        ) mustBe routes.SaoNameController.onPageLoad(NormalMode)
      }

      "when on IsThisTheSaoOnCertificatePage and the question is not answered then" in {
        intercept[NotImplementedError] {
          navigator.nextPage(
            IsThisTheSaoOnCertificatePage,
            NormalMode,
            UserAnswers("id")
          )
        }
      }

      "when on SaoNamePage, must go to SAO email page" in {
        navigator.nextPage(
          SaoNamePage,
          NormalMode,
          UserAnswers("id")
        ) mustBe routes.SaoEmailController.onPageLoad(NormalMode)
      }

      "when on SaoEmailPage, must go to SAO email communication choice page" in {
        navigator.nextPage(
          SaoEmailPage,
          NormalMode,
          UserAnswers("id")
        ) mustBe routes.SaoEmailCommunicationChoiceController.onPageLoad(NormalMode)
      }

      "when on SaoEmailCommunicationChoicePage, must go to certificate check your answers page" in {
        navigator.nextPage(
          SaoEmailCommunicationChoicePage,
          NormalMode,
          UserAnswers("id")
        ) mustBe routes.CertificateCheckYourAnswersController.onPageLoad()
      }

      "when on CertificateCheckYourAnswersPage, must go to who submits certificate page" in {
        navigator.nextPage(
          CertificateCheckYourAnswersPage,
          NormalMode,
          UserAnswers("id")
        ) mustBe routes.WhoSubmitsCertificateController.onPageLoad(NormalMode)
      }

      "when on WhoSubmitsCertificatePage, must go to qualified companies page" in {
        navigator.nextPage(
          WhoSubmitsCertificatePage,
          NormalMode,
          UserAnswers("id")
        ) mustBe routes.QualifiedCompaniesController.onPageLoad()
      }

      "when on QualifiedCompaniesPage, must go to unqualified companies page" in {
        navigator.nextPage(
          QualifiedCompaniesPage,
          NormalMode,
          UserAnswers("id")
        ) mustBe routes.UnqualifiedCompaniesController.onPageLoad()
      }

      "when on UnqualifiedCompaniesPage, must go to certificate submission declaration page" in {
        navigator.nextPage(
          UnqualifiedCompaniesPage,
          NormalMode,
          UserAnswers("id")
        ) mustBe routes.CertificateSubmissionDeclarationController.onPageLoad(NormalMode)
      }

      "when on CertificateSubmissionDeclarationPage, must go to certificate confirmation page" in {
        navigator.nextPage(
          CertificateSubmissionDeclarationPage,
          NormalMode,
          UserAnswers("id")
        ) mustBe routes.CertificateConfirmationController.onPageLoad()
      }

      "when on NotificationConfirmationPage, must go to certificate start page" in {
        navigator.nextPage(
          NotificationConfirmationPage,
          NormalMode,
          UserAnswers("id")
        ) mustBe routes.SubmitCertificateStartController.onPageLoad()
      }

      "when on UploadTemplateTablePage with no parsing errors, must go to submit notification start page" in {
        val userAnswers =
          UserAnswers("id")
            .set(UploadTemplateTablePage, UploadTemplateTableData(rows = Seq.empty, errors = Seq.empty))
            .success
            .value

        navigator.nextPage(
          UploadTemplateTablePage,
          NormalMode,
          userAnswers
        ) mustBe routes.SubmitNotificationStartController.onPageLoad()
      }

      "when on UploadTemplateTablePage with parsing errors, must go to upload form page" in {
        val userAnswers =
          UserAnswers("id")
            .set(
              UploadTemplateTablePage,
              UploadTemplateTableData(
                rows = Seq.empty,
                errors = Seq(models.upload.TemplateParseError(9, Some("Company UTR"), "missing_required_value", "x"))
              )
            )
            .success
            .value

        navigator.nextPage(
          UploadTemplateTablePage,
          NormalMode,
          userAnswers
        ) mustBe routes.NotificationUploadFormController.onPageLoad()
      }
    }

    "in Check mode" - {

      "when on NotificationAdditionalInformationPage, must go to notification check your answers page" in {
        navigator.nextPage(
          NotificationAdditionalInformationPage,
          CheckMode,
          UserAnswers("id")
        ) mustBe routes.NotificationCheckYourAnswersController.onPageLoad()
      }

      "when on SaoNamePage, must go to certificate check your answers page" in {
        navigator.nextPage(
          SaoNamePage,
          CheckMode,
          UserAnswers("id")
        ) mustBe routes.CertificateCheckYourAnswersController.onPageLoad()
      }

      "when on SaoEmailPage, must go to certificate check your answers page" in {
        navigator.nextPage(
          SaoEmailPage,
          CheckMode,
          UserAnswers("id")
        ) mustBe routes.CertificateCheckYourAnswersController.onPageLoad()
      }

      "when on SaoEmailCommunicationChoicePage, must go to certificate check your answers page" in {
        navigator.nextPage(
          SaoEmailCommunicationChoicePage,
          CheckMode,
          UserAnswers("id")
        ) mustBe routes.CertificateCheckYourAnswersController.onPageLoad()
      }

      "must throw an not-implemented error for an unspecified configuration" in {
        case object UnknownPage extends Page
        intercept[NotImplementedError] {
          navigator.nextPage(UnknownPage, CheckMode, UserAnswers("id"))
        }
      }

    }
  }
}
