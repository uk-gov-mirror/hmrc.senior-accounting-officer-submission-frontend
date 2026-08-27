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
import forms.notification.NotificationMultiSaoPreviousOfficerStartDateFormProvider
import models.Mode
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.data.Form
import views.html.notification.NotificationMultiSaoPreviousOfficerStartDateView

import java.time.LocalDate

import NotificationMultiSaoPreviousOfficerStartDateViewSpec.*

class NotificationMultiSaoPreviousOfficerStartDateViewSpec
    extends ViewSpecBase[NotificationMultiSaoPreviousOfficerStartDateView] {

  private val formProvider          = app.injector.instanceOf[NotificationMultiSaoPreviousOfficerStartDateFormProvider]
  private val form: Form[LocalDate] = formProvider()

  private def generateView(form: Form[LocalDate], mode: Mode): Document = {
    val view = SUT(saoName, form, mode, saoIndex)
    Jsoup.parse(view.toString)
  }

  "NotificationMultiSaoPreviousOfficerStartDateView" - {

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
            action = notificationRoutes.NotificationMultiSaoPreviousOfficerStartDateController.onSubmit(mode, saoIndex),
            buttonText = "Continue"
          )

          doc.createTestsWithOrWithoutError(hasError = false)

          doc.createTestsWithLargeCaption(pageCaption)

          doc.createTestMustShowHint(pageHint)
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

          doc.createTestsWithSubmissionButton(
            action = notificationRoutes.NotificationMultiSaoPreviousOfficerStartDateController.onSubmit(mode, saoIndex),
            buttonText = "Continue"
          )

          doc.createTestsWithOrWithoutError(hasError = false)

          doc.createTestsWithLargeCaption(pageCaption)

          doc.createTestMustShowHint(pageHint)
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

          doc.createTestsWithDateInput(
            values = DateFieldValues("", "", ""),
            hasError = true
          )

          doc.createTestsWithSubmissionButton(
            action = notificationRoutes.NotificationMultiSaoPreviousOfficerStartDateController.onSubmit(mode, saoIndex),
            buttonText = "Continue"
          )

          doc.createTestsWithOrWithoutError(hasError = true)

          doc.createTestsWithLargeCaption(pageCaption)

          doc.createTestMustShowHint(pageHint)
        }
      }
    }
  }
}

object NotificationMultiSaoPreviousOfficerStartDateViewSpec {
  val pageHeading = "When did Firstname Lastname become the SAO?"
  val pageTitle   = "What date did the previous SAO responsibility start? - Submit a notification"
  val pageCaption = "Submit a notification"
  val pageHint    = "For example 01 6 2024"

  val saoName  = "Firstname Lastname"
  val saoIndex = 3
}
