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
import config.AppConfig
import controllers.notification.routes as notificationRoutes
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import views.html.notification.NotificationConfirmationView

import NotificationConfirmationViewSpec.*

class NotificationConfirmationViewSpec extends ViewSpecBase[NotificationConfirmationView] {

  private def generateView(displayPdfLink: Boolean): Document =
    Jsoup.parse(SUT(testReferenceNumber, displayPdfLink).toString)

  "NotificationConfirmationView" - {
    "when displayPdfLink is true" - {
      val displayPdfLink = true
      AppConfig.setValue("hub-frontend.host", accountHomepageLinkUrl)
      val doc: Document = generateView(displayPdfLink)

      doc.createTestsWithStandardPageElements(
        pageTitle = pageTitle,
        pageHeading = pageHeading,
        showBackLink = false,
        showIsThisPageNotWorkingProperlyLink = true,
        hasError = false
      )

      "with a confirmation panel that" - {
        "must have the correct title" - {
          doc.getConfirmationPanel.getPanelTitle.createTestWithText(text = panelTitle)
        }

        "must have the correct body" - {
          doc.getConfirmationPanel.getPanelBody.createTestWithText(text = panelBody)
        }

        "must have the reference number in bold" in {
          val strongTags = doc.getConfirmationPanel.getPanelBody.select("strong")
          strongTags.size() mustBe 1
          strongTags.get(0).text() mustBe testReferenceNumber
        }
      }

      doc.createTestsWithParagraphs(
        pageParagraphs
      )

      doc.createTestsWithBulletPoints(
        pageListItemsWhenLinkDisplayed
      )

      doc.getMainContent
        .select("li span")
        .get(0)
        .createTestWithText(pageListItemsWhenLinkDisplayed(0))

      doc.getMainContent
        .select("li span")
        .get(1)
        .createTestWithText(pageListItemsWhenLinkDisplayed(1))

      doc.getMainContent
        .select("li span a")
        .get(0)
        .createTestWithLink(
          linkText = downloadPdfLinkText,
          destinationUrl = downloadPdfLinkUrl
        )

      doc.getMainContent
        .select("li span a")
        .get(1)
        .createTestWithLink(
          linkText = printPageLinkText,
          destinationUrl = "#"
        )

      doc.createTestsForSubHeadings(
        pageSubheadings
      )

      doc.createTestForInsetText(pageInsetText)

      doc.createTestsWithOrWithoutError(hasError = false)
      doc.createTestsWithSubmissionButton(
        action = notificationRoutes.NotificationConfirmationController.onSubmit(),
        buttonText = "Continue"
      )
    }

    "when displayPdfLink is false" - {
      val displayPdfLink = false
      AppConfig.setValue("hub-frontend.host", accountHomepageLinkUrl)
      val doc: Document = generateView(displayPdfLink)

      doc.createTestsWithStandardPageElements(
        pageTitle = pageTitle,
        pageHeading = pageHeading,
        showBackLink = false,
        showIsThisPageNotWorkingProperlyLink = true,
        hasError = false
      )

      "with a confirmation panel that" - {
        "must have the correct title" - {
          doc.getConfirmationPanel.getPanelTitle.createTestWithText(text = panelTitle)
        }

        "must have the correct body" - {
          doc.getConfirmationPanel.getPanelBody.createTestWithText(text = panelBody)
        }

        "must have the reference number in bold" in {
          val strongTags = doc.getConfirmationPanel.getPanelBody.select("strong")
          strongTags.size() mustBe 1
          strongTags.get(0).text() mustBe testReferenceNumber
        }
      }

      doc.createTestsWithParagraphs(
        pageParagraphs
      )

      doc.createTestsWithBulletPoints(
        pageListItemsWhenLinkNotDisplayed
      )

      doc.getMainContent
        .select("li span")
        .get(0)
        .createTestWithText(pageListItemsWhenLinkNotDisplayed(0))

      doc.getMainContent
        .select("li span a")
        .get(0)
        .createTestWithLink(
          linkText = printPageLinkText,
          destinationUrl = "#"
        )

      doc.createTestsForSubHeadings(
        pageSubheadings
      )

      doc.createTestForInsetText(pageInsetText)

      doc.createTestsWithOrWithoutError(hasError = false)
      doc.createTestsWithSubmissionButton(
        action = notificationRoutes.NotificationConfirmationController.onSubmit(),
        buttonText = "Continue"
      )
    }

  }

  extension (target: => Document) {
    def createTestsForSubHeadings(subheadings: Seq[String]): Unit = {
      val headings = target.getMainContent.getElementsByTag("h2")
      "must have expected number of headings" in {
        headings.size() mustBe subheadings.length
      }
      subheadings.zipWithIndex.foreach((subheading, i) => {
        s"must have heading '$subheading'" in {
          headings.get(i).text mustBe subheading
        }
      })
    }
  }

}

object NotificationConfirmationViewSpec {
  val pageHeading                 = "Notification submitted"
  val pageTitle                   = "Notification submitted - Submit a notification"
  val pageParagraphs: Seq[String] = Seq(
    "HMRC has received your notification. We’ve sent a confirmation email to all the contacts you provided during registration.",
    "To keep a record of your submission, you can:",
    "Someone from HMRC may contact you if they need more information.",
    "You can submit a certificate or another notification from your account homepage."
  )
  val panelTitle          = "Notification submitted"
  val testReferenceNumber = "SAONOT0123456789"
  val panelBody: String   = s"Your reference number $testReferenceNumber"
  val testDate            = "17 January 2025 at 14:15am (GMT)"

  val downloadAPdfPageListItem =
    "download a PDF to save a copy of all the answers. You may not be able to do this after you leave this page"
  val printThisPagePageListItem =
    "print this page to keep a paper copy of your confirmation"

  val pageListItemsWhenLinkDisplayed: Seq[String] =
    Seq(
      downloadAPdfPageListItem,
      printThisPagePageListItem
    )

  val pageListItemsWhenLinkNotDisplayed: Seq[String] =
    Seq(
      printThisPagePageListItem
    )

  val downloadPdfLinkText        = "download a PDF"
  val downloadPdfLinkUrl: String =
    s"/senior-accounting-officer/submission/notification/download?notificationReference=$testReferenceNumber"

  val printPageLinkText = "print this page"

  val accountHomepageLinkText = "account homepage"
  val accountHomepageLinkUrl  = "testHubUrl"

  val pageSubheadings: Seq[String] = Seq("What happens next")

  val pageInsetText =
    "If you later realise the information is incorrect, contact your Customer Compliance Manager (CCM) if you have one, or email wmbc.saomailbox@hmrc.gov.uk for support."
}
