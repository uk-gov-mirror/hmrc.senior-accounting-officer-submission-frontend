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

package views

import base.ViewSpecBase
import controllers.routes
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import views.TemplateGuidanceViewSpec.*
import views.html.TemplateGuidanceView

class TemplateGuidanceViewSpec extends ViewSpecBase[TemplateGuidanceView] {

  val doc: Document             = Jsoup.parse(SUT().toString)
  val docWithinNewTab: Document = Jsoup.parse(SUT(true).toString)
  val mainContent: Element      = doc.getMainContent

  private def generateView(): Document = {
    val view = SUT()
    Jsoup.parse(view.toString)
  }

  "TemplateGuidanceView" - {

    val doc = generateView()

    docWithinNewTab.createTestWithinNewTab()

    doc.createTestsWithStandardPageElements(
      pageTitle = pageTitle,
      pageHeading = pageHeading,
      showBackLink = true,
      showIsThisPageNotWorkingProperlyLink = true,
      hasError = false
    )

    doc.createTestsWithSubmissionButton(routes.SubmissionTypeController.onPageLoad(), submissionBtnText)

    doc.createTestsWithOrWithoutError(hasError = false)

    doc.createTestsWithParagraphs(paragraphs)

    doc.getMainContent
      .select("a.govuk-link")
      .get(0)
      .createTestWithLink(linkTexts, routes.DownloadTemplateController.downloadFile().url)

    doc.createTestsForSubHeadings(pageSubheadings)

    doc.createTestsWithBulletPoints(pageBullets)

    doc.createTestsWithNumberedItems(pageNumberedListItems)

    doc.createTestForInsetText(pageInsetText)
  }

  extension (target: => Document) {
    def createTestsForSubHeadings(subheadings: Seq[String]): Unit = {
      val headings = doc.getMainContent.getElementsByTag("h3")
      "must have expected number of headings" in {
        headings.size() mustBe subheadings.length
      }
      subheadings.zipWithIndex.foreach((subheading, i) => {
        s"must have heading '$subheading'" in {
          headings.get(i).text mustBe subheading
        }
      })
    }

    def createTestWithinNewTab(): Unit = {
      val btn = docWithinNewTab.getElementsByAttributeValue("id", "submit")

      docWithinNewTab.createTestWithBackLink(false)
      btn.size() mustBe 0
    }

  }
}

object TemplateGuidanceViewSpec {
  val pageTitle               = "Submission template guidance"
  val pageHeading             = "How to complete and submit your submission template"
  val paragraphs: Seq[String] = Seq(
    "Follow these steps to download a template, complete and submit your notification and certificate.",
    "Download a submission template",
    "Complete all fields for each company in your group. Each row should represent one company your SAO was responsible for in the financial year.",
    "Do not change the layout or structure of the template. If you do, the upload will fail. Rows 1 to 9 of the template have guidance to help you complete each column.",
    "You'll need to enter:",
    "If you have a qualified certificate, you must explain why in the template. Your explanation should include:",
    "You can also include any plans to address the issues, this is optional.",
    "You must complete the correct sections before uploading. The information we process from your template depends on which submission type you choose.",
    "If you choose to submit:",
    "When you’ve completed your template, follow these steps.",
    "If you submit a notification now and want to add your certificate later, complete all columns and upload the template again when you are ready.",
    "If the template you uploaded has errors, you will be shown what to fix. Open your Excel file and correct the errors. Then save it again as a CSV (comma delimited) (*.csv) file and upload it again.",
    "If there are no errors, you will see a summary of your information to review before you continue.",
    "Complete the remaining steps in the service. Your template will only be sent to HMRC once you have reviewed your answers and selected 'Confirm and submit'."
  )
  val pageSubheadings: Seq[String] = Seq(
    "Step 1: Download and complete a submission template",
    "Qualified certificate: what to include in your explanation",
    "Step 2: Upload your template",

    "Step 3: Check the information is correct",
    "Step 4: Complete your submission"
  )
  val pageNumberedListItems: Seq[String] = Seq(
    "Save it as a CSV (comma delimited) (*.csv) file",
    "Upload the file to start your submission"
  )
  val pageBullets: Seq[String] = Seq(
    "company name",
    "Company Registration Number (CRN)",
    "Unique Taxpayer Reference (UTR)",
    "financial year end",
    "organisation type",
    "company status",
    "certificate type",
    "an explanation of a qualified certificate and the tax regimes it relates to",
    "what went wrong with the tax accounting arrangements",
    "the reason the SAO decided to provide a qualified certificate",
    "what led to the errors, not just a list",
    "a notification only, we will process the notification details",
    "a certificate only, we will process both sections – the certificate needs your company details from the notification section to be complete",
    "a notification and a certificate together, we will process both sections"
  )
  val pageInsetText =
    "When you make a submission for more than one SAO, you must complete a separate template for each one."

  val linkTexts         = "Download a submission template"
  val submissionBtnText = "Make a submission"

}
