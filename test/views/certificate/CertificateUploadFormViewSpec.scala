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

package views.certificate

import base.ViewSpecBase
import controllers.routes
import forms.UploadFormProvider
import models.upscan.UpscanInitiateResponse
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.data.Form
import views.html.certificate.CertificateUploadFormView

import CertificateUploadFormViewSpec.*

class CertificateUploadFormViewSpec extends ViewSpecBase[CertificateUploadFormView] {

  private val formProvider       = app.injector.instanceOf[UploadFormProvider]
  private val form: Form[String] = formProvider()

  private def generateView(form: Form[String]): Document = {
    val view = SUT(form, upscanInitiateResponse)
    Jsoup.parse(view.toString)
  }

  "CertificateUploadFormView" - {
    "Page with no error" - {
      val doc: Document = generateView(form)

      doc.createTestsWithStandardPageElements(
        pageTitle = pageTitle,
        pageHeading = pageHeading,
        showBackLink = true,
        showIsThisPageNotWorkingProperlyLink = true,
        hasError = false
      )

      doc.createTestsWithOrWithoutError(hasError = false)

      "must contain hidden fields" in {
        val hiddenFields = doc.select("input[type=hidden]")
        hiddenFields.size() mustBe 2
        hiddenFields.get(0).attr("name") mustBe "test1"
        hiddenFields.get(1).attr("name") mustBe "test2"
        hiddenFields.get(0).attr("value") mustBe "testValue1"
        hiddenFields.get(1).attr("value") mustBe "testValue2"
      }

      doc.createTestsWithLargeCaption(captionText)

      doc.createTestsWithParagraphs(paragraphs)

      doc
        .getElementById("template-guidance")
        .createTestWithLink(
          "Read guidance on how to complete the submission template (opens in new tab)",
          routes.TemplateGuidanceController.onPageLoadNewTab().url
        )

      doc.createTestForInsetText(insetText)

      "must contain file upload input element" in {
        doc.select(s"input#$uploadFormInputId.govuk-file-upload").size() mustBe 1
      }

      "must restrict the file picker to CSV and XLSX files" in {
        val fileInput = doc.select(s"input#$uploadFormInputId")
        fileInput.attr("accept") mustBe ".csv,.xlsx"
      }

      "must contain label for file upload input element" in {
        val label = doc.select(s"""label.govuk-label[for="$uploadFormInputId"]""")
        label.size() mustBe 1
        label.text() mustBe uploadFormLabel
      }

    }

    "Page with error" - {
      val doc: Document = generateView(form.withError(uploadFormInputId, errorMessage))

      doc.createTestsWithStandardPageElements(
        pageTitle = pageTitle,
        pageHeading = pageHeading,
        showBackLink = true,
        showIsThisPageNotWorkingProperlyLink = true,
        hasError = true
      )

      doc.createTestsWithOrWithoutError(hasError = true)

      "must contain hidden fields" in {
        val hiddenFields = doc.select("input[type=hidden]")
        hiddenFields.size() mustBe 2
        hiddenFields.get(0).attr("name") mustBe "test1"
        hiddenFields.get(1).attr("name") mustBe "test2"
        hiddenFields.get(0).attr("value") mustBe "testValue1"
        hiddenFields.get(1).attr("value") mustBe "testValue2"
      }

      doc.createTestsWithParagraphs(paragraphs)

      doc
        .getElementById("template-guidance")
        .createTestWithLink(
          "Read guidance on how to complete the submission template (opens in new tab)",
          routes.TemplateGuidanceController.onPageLoadNewTab().url
        )

      doc.createTestForInsetText(insetText)

      "must contain file upload input element" in {
        doc.select(s"input#$uploadFormInputId.govuk-file-upload").size() mustBe 1
      }

      "must contain label for file upload input element" in {
        val label = doc.select(s"""label.govuk-label[for="$uploadFormInputId"]""")
        label.size() mustBe 1
        label.text() mustBe uploadFormLabel
      }
    }
  }
}

object CertificateUploadFormViewSpec {
  val pageHeading = "Upload a submission template"
  val pageTitle   = "Upload a submission template for your certificate"

  val captionText = "Submit a certificate"

  val paragraphs: List[String] = List(
    "The template must be uploaded in CSV format. If your template is saved as .xls or .xlsx, save it again as a CSV (comma delimited) (.csv) file before uploading.",
    "Read guidance on how to complete the submission template (opens in new tab)"
  )

  val insetText =
    "If your group has more than one SAO, you must submit a separate certificate for each one. After you complete this submission, you can start another certificate."

  val uploadFormLabel   = "Upload a file"
  val uploadFormInputId = "file-input"

  val upscanInitiateResponse: UpscanInitiateResponse = UpscanInitiateResponse(
    reference = "testReference",
    postTarget = "formPostTarget",
    formFields = Map(
      "test1" -> "testValue1",
      "test2" -> "testValue2"
    )
  )

  val errorMessage = "Example error message"
}
