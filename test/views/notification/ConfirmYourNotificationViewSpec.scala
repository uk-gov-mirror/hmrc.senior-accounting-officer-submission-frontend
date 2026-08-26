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
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import views.html.notification.ConfirmYourNotificationView

import ConfirmYourNotificationViewSpec.*

class ConfirmYourNotificationViewSpec extends ViewSpecBase[ConfirmYourNotificationView] {

  private def generateView(): Document = Jsoup.parse(SUT().toString)

  "ConfirmYourNotificationView" - {
    val doc: Document = generateView()

    doc.createTestsWithStandardPageElements(
      pageTitle = pageTitle,
      pageHeading = pageHeading,
      showBackLink = true,
      showIsThisPageNotWorkingProperlyLink = true,
      hasError = false
    )

    doc.createTestsWithOrWithoutError(hasError = false)
    doc.createTestsWithLargeCaption(pageCaption)
    doc.createTestsWithParagraphs(pageParagraphs)
    doc.createTestsWithBulletPoints(pageBullets)
  }
}

object ConfirmYourNotificationViewSpec {
  val pageHeading = "Confirm your notification"
  val pageTitle   = "Confirm your notification - Submit a notification"
  val pageCaption = "Submit a notification"

  val pageParagraphs: Seq[String] = Seq(
    "This is an official notification to HMRC about the Senior Accounting Officer (SAO) requirement under Schedule 46 of the Finance Act 2009.",
    "By submitting this notification, you confirm that:"
  )

  val pageBullets: Seq[String] = Seq(
    "the information you have provided is complete and correct to the best of your knowledge",
    "if the company deliberately provides false or incomplete information, or fails to report changes, it may be liable for a £5,000 penalty"
  )
}
