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
import models.NormalMode
import models.notification.NotificationStage
import models.notification.NotificationStage.{SubmitNotificationInfo, UploadSubmissionTemplateDetails}
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import views.html.notification.NotificationTaskListView

import NotificationTaskListViewSpec.*

class NotificationTaskListViewSpec extends ViewSpecBase[NotificationTaskListView] {

  private def generateView(stage: NotificationStage): Document = Jsoup.parse(SUT(stage).toString)

  "NotificationTaskListView" - {

    "NotificationTaskListView with 'provide sao details' stage" - {

      val doc: Document = generateView(NotificationStage.ProvideSaoDetails)
      doc.createTestsWithStandardPageElements(
        pageTitle = pageTitle,
        pageHeading = pageHeading,
        showBackLink = false,
        showIsThisPageNotWorkingProperlyLink = true,
        hasError = false
      )

      doc.createTestsWithParagraphs(paragraphs)

      doc.createTestsWithOrWithoutError(hasError = false)

      "must render task list data-test-ids" in {
        val taskNames = doc.getMainContent.getElementsByClass("govuk-task-list__name-and-hint")
        taskNames.size() mustBe 3

        taskNames.get(0).attr("data-test-id") mustBe "provide-sao-details"
        taskNames.get(1).attr("data-test-id") mustBe "upload-submission-template"
        taskNames.get(2).attr("data-test-id") mustBe "submit-notification"
      }

      "must render task list status ids" in {
        val statusTags = doc.getMainContent.getElementsByClass("govuk-task-list__status")
        statusTags.size() mustBe 3

        statusTags.get(0).id() mustBe "provide-sao-details-status"
        statusTags.get(1).id() mustBe "upload-template-details-status"
        statusTags.get(2).id() mustBe "submit-notification-details-status"
      }

      doc.getMainContent
        .select("a.govuk-link")
        .get(0)
        .createTestWithLink(
          linkText = provideSaoDetailsLinkText,
          destinationUrl = notificationRoutes.NotificationMoreThanOneSaoController.onPageLoad(NormalMode).url
        )

      "must show the correct statuses" in {
        val statusTags = doc.getMainContent.getElementsByClass("govuk-task-list__status")
        statusTags.size() mustBe 3

        val provideSaoDetailsTag  = statusTags.get(0)
        val uploadNotificationTag = statusTags.get(1)
        val submitNotificationTag = statusTags.get(2)

        provideSaoDetailsTag.text() mustBe notStartedText
        provideSaoDetailsTag.getElementsByTag("strong").attr("class") mustBe "govuk-tag govuk-tag--blue"
        uploadNotificationTag.text() mustBe cannotStartText
        submitNotificationTag.text() mustBe cannotStartText
      }
    }

    "NotificationTaskListView with 'upload template' stage" - {

      val doc: Document = generateView(UploadSubmissionTemplateDetails)
      doc.createTestsWithStandardPageElements(
        pageTitle = pageTitle,
        pageHeading = pageHeading,
        showBackLink = false,
        showIsThisPageNotWorkingProperlyLink = true,
        hasError = false
      )

      doc.createTestsWithParagraphs(paragraphs)

      doc.createTestsWithOrWithoutError(hasError = false)

      doc.getMainContent
        .select("a.govuk-link")
        .get(0)
        .createTestWithLink(
          linkText = uploadTemplateLinkText,
          destinationUrl = notificationRoutes.NotificationUploadFormController.onPageLoad().url
        )

      "must show the correct statuses" in {
        val statusTags = doc.getMainContent.getElementsByClass("govuk-task-list__status")
        statusTags.size() mustBe 3

        val provideSaoDetailsTag  = statusTags.get(0)
        val uploadNotificationTag = statusTags.get(1)
        val submitNotificationTag = statusTags.get(2)

        provideSaoDetailsTag.text() mustBe completedText
        uploadNotificationTag.text() mustBe notStartedText
        uploadNotificationTag.getElementsByTag("strong").attr("class") mustBe "govuk-tag govuk-tag--blue"
        submitNotificationTag.text() mustBe cannotStartText
      }
    }

    "NotificationTaskListView with 'submit notification' stage" - {
      val doc: Document = generateView(SubmitNotificationInfo)
      doc.createTestsWithStandardPageElements(
        pageTitle = pageTitle,
        pageHeading = pageHeading,
        showBackLink = false,
        showIsThisPageNotWorkingProperlyLink = true,
        hasError = false
      )

      doc.createTestsWithParagraphs(paragraphs)

      doc.createTestsWithOrWithoutError(hasError = false)

      doc.getMainContent
        .select("a.govuk-link")
        .get(0)
        .createTestWithLink(
          linkText = submitNotificationLinkText,
          destinationUrl = notificationRoutes.NotificationAdditionalInformationController.onPageLoad(NormalMode).url
        )

      "must show the correct statuses" in {
        val statusTags = doc.getMainContent.getElementsByClass("govuk-task-list__status")
        statusTags.size() mustBe 3

        val provideSaoDetailsTag  = statusTags.get(0)
        val uploadNotificationTag = statusTags.get(1)
        val submitNotificationTag = statusTags.get(2)

        provideSaoDetailsTag.text() mustBe completedText
        uploadNotificationTag.text() mustBe completedText
        submitNotificationTag.text() mustBe notStartedText
        submitNotificationTag.getElementsByTag("strong").attr("class") mustBe "govuk-tag govuk-tag--blue"
      }
    }
  }

  "NotificationTaskListView with all stages complete" - {
    val doc: Document = generateView(NotificationStage.AllStagesCompleted)
    doc.createTestsWithStandardPageElements(
      pageTitle = pageTitle,
      pageHeading = pageHeading,
      showBackLink = false,
      showIsThisPageNotWorkingProperlyLink = true,
      hasError = false
    )

    doc.createTestsWithParagraphs(paragraphs)

    doc.createTestsWithOrWithoutError(hasError = false)

    "must show the correct statuses" in {
      val statusTags = doc.getMainContent.getElementsByClass("govuk-task-list__status")
      statusTags.size() mustBe 3

      val provideSaoDetailsTag  = statusTags.get(0)
      val uploadNotificationTag = statusTags.get(1)
      val submitNotificationTag = statusTags.get(2)

      provideSaoDetailsTag.text() mustBe completedText
      uploadNotificationTag.text() mustBe completedText
      submitNotificationTag.text() mustBe completedText
    }

    doc.createTestsWithSubmissionButton(
      action = notificationRoutes.NotificationTaskListController.onCompleteSubmit(),
      buttonText = homepageBtnText
    )
  }

}

object NotificationTaskListViewSpec {
  val pageHeading             = "Submit a notification"
  val pageTitle               = "Submit a notification"
  val paragraphs: Seq[String] = Seq(
    "Submit one notification for each SAO in your organisation. Each notification must include the SAO’s details and all the companies they were responsible for in a previous financial year."
  )
  val provideSaoDetailsLinkText  = "Provide the SAO’s details"
  val uploadTemplateLinkText     = "Upload the submission template"
  val submitNotificationLinkText = "Submit the notification"

  val notStartedText  = "Not started"
  val cannotStartText = "Cannot start yet"
  val completedText   = "Completed"
  val homepageBtnText = "Go back to the homepage"
}
