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
import base.ViewSpecBase.DateFieldValues
import controllers.notification.routes as notificationRoutes
import forms.notification.NotificationMultiSaoLastOfficerStartDateFormProvider
import models.Mode
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.data.Form
import views.html.notification.NotificationMultiSaoLastOfficerStartDateView

import java.time.LocalDate

import NotificationMultiSaoLastOfficerStartDateViewSpec.*

class NotificationMultiSaoLastOfficerStartDateViewSpec
    extends ViewSpecBase[NotificationMultiSaoLastOfficerStartDateView] {

  private val formProvider          = app.injector.instanceOf[NotificationMultiSaoLastOfficerStartDateFormProvider]
  private val form: Form[LocalDate] = formProvider()

  private def generateView(form: Form[LocalDate], mode: Mode): Document = {
    val view = SUT("Firstname Lastname", form, mode)
    Jsoup.parse(view.toString)
  }

  "NotificationMultiSaoLastOfficerStartDateView" - {

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

          doc.createTestsWithDateInput(
            values = DateFieldValues("", "", ""),
            hasError = false
          )

          doc.createTestsWithSubmissionButton(
            action = notificationRoutes.NotificationMultiSaoLastOfficerStartDateController.onSubmit(mode),
            buttonText = "Continue"
          )

          doc.createTestMustShowHint(pageHint)

          doc.createTestsWithOrWithoutError(hasError = false)
        }

        "when the form is filled in" - {
          val doc = generateView(form.bind(Map("value.day" -> "1", "value.month" -> "1", "value.year" -> "2000")), mode)

          doc.createTestsWithStandardPageElements(
            pageTitle = pageTitle,
            pageHeading = pageHeading,
            showBackLink = true,
            showIsThisPageNotWorkingProperlyLink = true,
            hasError = false
          )

          doc.createTestsWithDateInput(
            values = DateFieldValues("1", "1", "2000"),
            hasError = false
          )

          doc.createTestMustShowHint(pageHint)

          doc.createTestsWithSubmissionButton(
            action = notificationRoutes.NotificationMultiSaoLastOfficerStartDateController.onSubmit(mode),
            buttonText = "Continue"
          )

          doc.createTestsWithOrWithoutError(hasError = false)
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

          doc.createTestMustShowHint(pageHint)

          doc.createTestsWithDateInput(
            values = DateFieldValues("", "", ""),
            hasError = true
          )

          doc.createTestsWithSubmissionButton(
            action = notificationRoutes.NotificationMultiSaoLastOfficerStartDateController.onSubmit(mode),
            buttonText = "Continue"
          )

          doc.createTestsWithOrWithoutError(hasError = true)
        }
      }
    }
  }
}

object NotificationMultiSaoLastOfficerStartDateViewSpec {
  val pageHeading = "What date did Firstname Lastname become the SAO?"
  val pageTitle   = "What date did the last SAO become the SAO? - Submit a notification"
  val pageCaption = "Submit a notification"
  val pageHint    = "For example 01 6 2024"
}
