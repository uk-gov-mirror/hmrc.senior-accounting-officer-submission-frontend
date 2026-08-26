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

package views.notification

import base.ViewSpecBase
import controllers.notification.routes as notificationRoutes
import forms.notification.NotificationAdditionalInformationFormProvider
import models.Mode
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.scalactic.source.Position
import play.api.data.Form
import views.html.notification.NotificationAdditionalInformationView

import NotificationAdditionalInformationViewSpec.*

class NotificationAdditionalInformationViewSpec extends ViewSpecBase[NotificationAdditionalInformationView] {

  private val formProvider               = app.injector.instanceOf[NotificationAdditionalInformationFormProvider]
  private val form: Form[Option[String]] = formProvider()

  private def generateView(form: Form[Option[String]], mode: Mode): Document = {
    val view = SUT(form, mode)
    Jsoup.parse(view.toString)
  }

  "NotificationAdditionalInformationView" - {

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

          doc.createTestMustShowNumberOfTextareas(1)
          doc.createTestMustShowTextarea(
            name = "value",
            label = textAreaLabel,
            value = "",
            hint = None,
            hasError = false
          )

          doc.createTestsWithLargeCaption(pageCaption)

          doc.createTestsWithSubmissionButtons(
            action = notificationRoutes.NotificationAdditionalInformationController.onSubmit(mode),
            buttonTexts = Seq("Continue", "Skip")
          )

          doc.createTestsWithOrWithoutError(
            hasError = false
          )
        }

        "when the form is filled in" - {
          val doc = generateView(form.bind(Map("value" -> testInputValue)), mode)

          doc.createTestsWithStandardPageElements(
            pageTitle = pageTitle,
            pageHeading = pageHeading,
            showBackLink = true,
            showIsThisPageNotWorkingProperlyLink = true,
            hasError = false
          )

          doc.createTestsWithParagraphs(
            paragraphs
          )

          doc.createTestsWithBulletPoints(
            bulletPoints
          )

          doc.createTestMustShowNumberOfTextareas(1)

          doc.createTestMustShowTextarea(
            name = "value",
            label = textAreaLabel,
            value = testInputValue,
            hint = None,
            hasError = false
          )

          doc.createTestsWithSubmissionButtons(
            action = notificationRoutes.NotificationAdditionalInformationController.onSubmit(mode),
            buttonTexts = Seq("Continue", "Skip")
          )

          doc.createTestsWithOrWithoutError(
            hasError = false
          )
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

          doc.createTestsWithLargeCaption(pageCaption)

          doc.createTestMustShowTextarea(
            name = "value",
            label = textAreaLabel,
            value = "",
            hint = None,
            hasError = true
          )

          doc.createTestsWithSubmissionButtons(
            action = notificationRoutes.NotificationAdditionalInformationController.onSubmit(mode),
            buttonTexts = Seq("Continue", "Skip")
          )

          doc.createTestsWithOrWithoutError(
            hasError = true
          )
        }
      }
    }
  }
}

object NotificationAdditionalInformationViewSpec {
  val pageCaption             = "Submit a notification"
  val pageHeading             = "Additional information about your notification"
  val pageTitle               = "Additional information about your notification - Submit a notification"
  val testInputValue          = "myTestInputValue"
  val paragraphs: Seq[String] = Seq(
    "Tell us if there is anything we should know about your notification.",
    "This could include:"
  )
  val bulletPoints: Seq[String] = Seq(
    "a company’s status changing, such as becoming dormant or going into liquidation",
    "anything else relevant to the companies listed"
  )
  val textAreaLabel = "Is there anything else we should know about your notification?"
}
