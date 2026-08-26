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

package views.notification

import base.ViewSpecBase
import controllers.notification.routes as notificationRoutes
import forms.notification.NotificationMoreThanOneSaoFormProvider
import models.Mode
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.data.Form
import views.html.notification.NotificationMoreThanOneSaoView

import NotificationMoreThanOneSaoViewSpec.*

class NotificationMoreThanOneSaoViewSpec extends ViewSpecBase[NotificationMoreThanOneSaoView] {

  private val formProvider        = app.injector.instanceOf[NotificationMoreThanOneSaoFormProvider]
  private val form: Form[Boolean] = formProvider()

  private def generateView(form: Form[Boolean], mode: Mode): Document = {
    val view = SUT(form, mode)
    Jsoup.parse(view.toString)
  }

  "NotificationMoreThanOneSaoView" - {

    Mode.values.foreach { mode =>
      s"when using $mode" - {
        "when the form is not filled in" - {
          val doc = generateView(form, mode)

          doc.createTestsWithStandardPageElements(
            pageTitle = pageTitle,
            pageHeading = pageHeading,
            showBackLink = true,
            showIsThisPageNotWorkingProperlyLink = true,
            hasError = false
          )

          "must have a back link to the submit notification task list" in {
            doc.getElementsByClass("govuk-back-link").attr("href") mustBe
              notificationRoutes.NotificationTaskListController.onPageLoad().url
          }

          doc.createTestsWithRadioButtons(
            name = "value",
            radios = List(
              radio(value = yesKey, label = yesLabel, hint = None),
              radio(value = noKey, label = noLabel, hint = None)
            ),
            isChecked = None,
            hasError = false
          )

          doc.createTestsWithSubmissionButton(
            action = notificationRoutes.NotificationMoreThanOneSaoController.onSubmit(mode),
            buttonText = "Continue"
          )

          doc.createTestsWithOrWithoutError(
            hasError = false
          )

          doc.createTestsWithLargeCaption(pageCaption)
        }

        "when the form is filled in" - {
          val doc = generateView(form.bind(Map("value" -> yesKey)), mode)

          doc.createTestsWithStandardPageElements(
            pageTitle = pageTitle,
            pageHeading = pageHeading,
            showBackLink = true,
            showIsThisPageNotWorkingProperlyLink = true,
            hasError = false
          )

          doc.createTestsWithRadioButtons(
            name = "value",
            radios = List(
              radio(value = yesKey, label = yesLabel, hint = None),
              radio(value = noKey, label = noLabel, hint = None)
            ),
            isChecked = Some(radio(value = yesKey, label = yesLabel, hint = None)),
            hasError = false
          )

          doc.createTestsWithSubmissionButton(
            action = notificationRoutes.NotificationMoreThanOneSaoController.onSubmit(mode),
            buttonText = "Continue"
          )

          doc.createTestsWithOrWithoutError(
            hasError = false
          )

          doc.createTestsWithLargeCaption(pageCaption)
        }

        "when the form has errors" - {
          val doc = generateView(form.withError("value", "broken"), mode)

          doc.createTestsWithStandardPageElements(
            pageTitle = pageTitle,
            pageHeading = pageHeading,
            showBackLink = true,
            showIsThisPageNotWorkingProperlyLink = true,
            hasError = true
          )

          doc.createTestsWithRadioButtons(
            name = "value",
            radios = List(
              radio(value = yesKey, label = yesLabel, hint = None),
              radio(value = noKey, label = noLabel, hint = None)
            ),
            isChecked = None,
            hasError = true
          )

          doc.createTestsWithSubmissionButton(
            action = notificationRoutes.NotificationMoreThanOneSaoController.onSubmit(mode),
            buttonText = "Continue"
          )

          doc.createTestsWithOrWithoutError(
            hasError = true
          )

          doc.createTestsWithLargeCaption(pageCaption)
        }
      }
    }

  }

}

object NotificationMoreThanOneSaoViewSpec {
  val pageHeading = "Did the SAO change during the financial year you are submitting for?"
  val pageTitle   = "Did the SAO change during the financial year you are submitting for? - Submit a notification"
  val yesKey      = "true"
  val yesLabel    = "Yes"
  val noKey       = "false"
  val noLabel     = "No"
  val yesHint     = "There was more than one SAO, the SAO changed during the financial year"
  val noHint      = "Only one person held the role for the entire financial year"
  val pageCaption = "Submit a notification"
}
