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
import controllers.notification.routes as notificationRoutes
import controllers.routes
import models.*
import models.upload.UploadTemplateTableData
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import pages.*
import pages.notification.*
import services.csvparser.UploadTemplateCsvSchema.{Column, TemplateError}

import java.time.LocalDate

class NotificationNavigatorSpec extends SpecBase with GuiceOneAppPerSuite {

  lazy val navigator: Navigator = app.injector.instanceOf[NotificationNavigator]

  "NotificationNavigator.nextPage" - {

    "in Normal mode" - {

      "must throw an not-implemented error for an unspecified configuration" in {
        case object UnknownPage extends Page
        intercept[NotImplementedError] {
          navigator.nextPage(UnknownPage, NormalMode, emptyUserAnswers)
        }
      }

      "when on NotificationAdditionalInformationPage, must go to confirm your notification page" in {
        navigator.nextPage(
          NotificationAdditionalInformationPage,
          NormalMode,
          emptyUserAnswers
        ) mustBe notificationRoutes.ConfirmYourNotificationController.onPageLoad()
      }

      "when on ConfirmYourNotificationPage, must go to check your answers page" in {
        navigator.nextPage(
          ConfirmYourNotificationPage,
          NormalMode,
          emptyUserAnswers
        ) mustBe notificationRoutes.NotificationCheckYourAnswersController.onPageLoad()
      }

      "when on ConfirmYourNotificationPage, must go to notification check your answers page" in {
        navigator.nextPage(
          ConfirmYourNotificationPage,
          NormalMode,
          emptyUserAnswers
        ) mustBe notificationRoutes.NotificationCheckYourAnswersController.onPageLoad()
      }

      "when on NotificationConfirmationPage, must go to notification task list" in {
        navigator.nextPage(
          NotificationConfirmationPage,
          NormalMode,
          emptyUserAnswers
        ) mustBe notificationRoutes.NotificationTaskListController.onComplete()
      }

      "when on NotificationMoreThanOneSaoPage and the user selected No, must go to Sao name page" in {
        navigator.nextPage(
          NotificationMoreThanOneSaoPage,
          NormalMode,
          emptyUserAnswers.set(NotificationMoreThanOneSaoPage, false).success.value
        ) mustBe notificationRoutes.NotificationSingleSaoOfficerNameController.onPageLoad(NormalMode)
      }

      "when on NotificationMoreThanOneSaoPage and the user selected Yes, must go to multiple sao name page" in {
        navigator.nextPage(
          NotificationMoreThanOneSaoPage,
          NormalMode,
          emptyUserAnswers.set(NotificationMoreThanOneSaoPage, true).success.value
        ) mustBe notificationRoutes.NotificationMultiSaoLastOfficerNameController.onPageLoad(NormalMode)
      }

      "when on NotificationMultiSaoLastOfficerNameController, must go to more sao submit notification first date page" in {
        navigator.nextPage(
          NotificationMultiSaoLastOfficerNamePage,
          NormalMode,
          emptyUserAnswers.set(NotificationMoreThanOneSaoPage, true).success.value
        ) mustBe notificationRoutes.NotificationMultiSaoLastOfficerStartDateController.onPageLoad(NormalMode)
      }

      "when on NotificationMultiSaoLastOfficerStartDatePage, must go to who was the sao before page" in {
        navigator.nextPage(
          NotificationMultiSaoLastOfficerStartDatePage,
          NormalMode,
          emptyUserAnswers.set(NotificationMultiSaoLastOfficerStartDatePage, LocalDate.of(2026, 5, 1)).success.value
        ) mustBe notificationRoutes.NotificationMultiSaoPreviousOfficerNameController.onPageLoad(NormalMode)
      }

      "when on NotificationMultiSaoPreviousOfficerNamePage, must go to NotificationMultiSaoPreviousOfficerStartDate" in {
        navigator.nextPage(
          NotificationMultiSaoPreviousOfficerNamePage(0),
          NormalMode,
          emptyUserAnswers
        ) mustBe notificationRoutes.NotificationMultiSaoPreviousOfficerStartDateController.onPageLoad(NormalMode, 0)
      }

      "when on NotificationMultiSaoPreviousOfficerStartDatePage, must go to NotificationMultiSaoPreviousOfficerEndDate page" in {
        navigator.nextPage(
          NotificationMultiSaoPreviousOfficerStartDatePage(0),
          NormalMode,
          emptyUserAnswers
        ) mustBe notificationRoutes.NotificationMultiSaoPreviousOfficerEndDateController.onPageLoad(NormalMode)
      }

      "when on NotificationMultiSaoPreviousOfficerEndDatePage, must go to NotificationMultiSaoAreAllAdded page" in {
        navigator.nextPage(
          NotificationMultiSaoPreviousOfficerEndDatePage(0),
          NormalMode,
          emptyUserAnswers
        ) mustBe notificationRoutes.NotificationMultiSaoAreAllAddedController.onPageLoad(NormalMode)
      }

      "when on NotificationMultiSaoAreAllAddedPage, and no response is in the database, must throw an exception" in {
        intercept[NotImplementedError] {
          navigator.nextPage(
            NotificationMultiSaoAreAllAddedPage(0),
            NormalMode,
            emptyUserAnswers
          )
        }
      }

      "when on NotificationMultiSaoAreAllAddedPage, and the user answers yes, must go to the notification task list" in {
        navigator.nextPage(
          NotificationMultiSaoAreAllAddedPage(0),
          NormalMode,
          emptyUserAnswers.set(NotificationMultiSaoAreAllAddedPage(0), true).success.value
        ) mustBe notificationRoutes.NotificationTaskListController.onPageLoad()
      }

      "when on NotificationMultiSaoAreAllAddedPage, and the user answers no, must go to NotificationMultiSaoPreviousOfficerName page with an incremented saoIndex" in {
        navigator.nextPage(
          NotificationMultiSaoAreAllAddedPage(0),
          NormalMode,
          emptyUserAnswers.set(NotificationMultiSaoAreAllAddedPage(0), false).success.value
        ) mustBe notificationRoutes.NotificationMultiSaoPreviousOfficerNameController.onPageLoad(NormalMode, 1)
      }

      "when on NotificationSingleSaoOfficerNamePage, must go to the submit notification start page" in {
        navigator.nextPage(
          NotificationSingleSaoOfficerNamePage,
          NormalMode,
          emptyUserAnswers.set(NotificationSingleSaoOfficerNamePage, "Firstname Lastname").success.value
        ) mustBe notificationRoutes.NotificationTaskListController.onPageLoad()
      }

      "when on UploadTemplateTablePage with no parsing errors, must go to notification start page" in {
        val userAnswers =
          emptyUserAnswers
            .set(UploadTemplateTablePage, UploadTemplateTableData(rows = Seq.empty, errors = Seq.empty))
            .success
            .value

        navigator.nextPage(
          UploadTemplateTablePage,
          NormalMode,
          userAnswers
        ) mustBe notificationRoutes.NotificationTaskListController.onPageLoad()
      }

      "when on UploadTemplateTablePage with parsing errors, must go to upload form page" in {
        val userAnswers =
          emptyUserAnswers
            .set(
              UploadTemplateTablePage,
              UploadTemplateTableData(
                rows = Seq.empty,
                errors = Seq(models.upload.TemplateParseError(9, Some(Column.Utr), TemplateError.UtrError))
              )
            )
            .success
            .value

        navigator.nextPage(
          UploadTemplateTablePage,
          NormalMode,
          userAnswers
        ) mustBe notificationRoutes.NotificationUploadFormController.onPageLoad()
      }

      "when on UploadTemplateTablePage with no upload data, must go to journey recovery page" in {
        navigator.nextPage(
          UploadTemplateTablePage,
          NormalMode,
          emptyUserAnswers
        ) mustBe routes.JourneyRecoveryController.onPageLoad()
      }

    }

    "in Check mode" - {

      "when on NotificationAdditionalInformationPage, must go to notification check your answers page" in {
        navigator.nextPage(
          NotificationAdditionalInformationPage,
          CheckMode,
          emptyUserAnswers
        ) mustBe notificationRoutes.NotificationCheckYourAnswersController.onPageLoad()
      }

      "must throw an not-implemented error for an unspecified configuration" in {
        case object UnknownPage extends Page
        intercept[NotImplementedError] {
          navigator.nextPage(UnknownPage, CheckMode, emptyUserAnswers)
        }
      }

    }
  }
}
