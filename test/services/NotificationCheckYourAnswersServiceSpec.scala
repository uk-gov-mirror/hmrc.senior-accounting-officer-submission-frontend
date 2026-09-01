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

package services

import base.SpecBase
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import pages.notification.{
  NotificationAdditionalInformationPage,
  NotificationMoreThanOneSaoPage,
  NotificationSingleSaoOfficerNamePage
}
import play.api.i18n.{Messages, MessagesApi}
import play.api.test.FakeRequest
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import services.NotificationCheckYourAnswersServiceSpec.*
import pages.notification.NotificationMultiSaoLastOfficerNamePage
import pages.notification.NotificationMultiSaoLastOfficerStartDatePage
import java.time.LocalDate
import pages.notification.NotificationMultiSaoPreviousOfficerNamePage
import pages.notification.NotificationMultiSaoPreviousOfficerStartDatePage
import pages.notification.NotificationMultiSaoAreAllAddedPage
import pages.notification.NotificationMultiSaoPreviousOfficerEndDatePage

class NotificationCheckYourAnswersServiceSpec extends SpecBase with GuiceOneAppPerSuite {

  def SUT: NotificationCheckYourAnswersService = app.injector.instanceOf[NotificationCheckYourAnswersService]

  given Messages = app.injector.instanceOf[MessagesApi].preferred(FakeRequest())

  "Single Sao" - {
    "additional information provided" in {
      val userAnswers = emptyUserAnswers
        .set(NotificationMoreThanOneSaoPage, false)
        .get
        .set(NotificationSingleSaoOfficerNamePage, singleSaoName)
        .get
        .set(NotificationAdditionalInformationPage, Some(additionalInformation))
        .get
        .set(NotificationMultiSaoAreAllAddedPage(0), true)
        .get

      val result = SUT.getSummaryList(userAnswers)

      result.rows.length mustBe 3
      result.rows(0).key.content mustBe Text(SaoChange.key)
      result.rows(0).value.content mustBe HtmlContent(SaoChange.noContent)
      result.rows(1).key.content mustBe Text(SaoName.key)
      result.rows(1).value.content mustBe HtmlContent(SaoName.content)
      result.rows(2).key.content mustBe Text(AdditionalInformation.key)
      result.rows(2).value.content mustBe HtmlContent(AdditionalInformation.providedContent)
    }

    "additional information not provided" in {
      val userAnswers = emptyUserAnswers
        .set(NotificationMoreThanOneSaoPage, false)
        .get
        .set(NotificationSingleSaoOfficerNamePage, singleSaoName)
        .get

      val result = SUT.getSummaryList(userAnswers)

      result.rows.length mustBe 3
      result.rows(0).key.content mustBe Text(SaoChange.key)
      result.rows(0).value.content mustBe HtmlContent(SaoChange.noContent)
      result.rows(1).key.content mustBe Text(SaoName.key)
      result.rows(1).value.content mustBe HtmlContent(SaoName.content)
      result.rows(2).key.content mustBe Text(AdditionalInformation.key)
      result.rows(2).value.content mustBe HtmlContent(AdditionalInformation.notProvidedContent)
    }
  }

  "Multi Sao" - {
    "additional information provided" in {
      val userAnswers = emptyUserAnswers
        .set(NotificationMoreThanOneSaoPage, true)
        .get
        .set(NotificationAdditionalInformationPage, Some(additionalInformation))
        .get
        .set(NotificationMultiSaoLastOfficerNamePage, multiSaoName1)
        .get
        .set(NotificationMultiSaoLastOfficerStartDatePage, multiSao1StartDate)
        .get
        .set(NotificationMultiSaoPreviousOfficerNamePage(0), multiSaoName2)
        .get
        .set(NotificationMultiSaoPreviousOfficerStartDatePage(0), multiSao2StartDate)
        .get
        .set(NotificationMultiSaoPreviousOfficerEndDatePage(0), multiSao2EndDate)
        .get
        .set(NotificationMultiSaoAreAllAddedPage(0), false)
        .get
        .set(NotificationMultiSaoPreviousOfficerNamePage(1), multiSaoName3)
        .get
        .set(NotificationMultiSaoPreviousOfficerStartDatePage(1), multiSao3StartDate)
        .get
        .set(NotificationMultiSaoPreviousOfficerEndDatePage(1), multiSao3EndDate)
        .get
        .set(NotificationMultiSaoAreAllAddedPage(1), true)
        .get

      val result = SUT.getSummaryList(userAnswers)

      result.rows.length mustBe 12
      result.rows(0).key.content mustBe Text(SaoChange.key)
      result.rows(0).value.content mustBe HtmlContent(SaoChange.yesContent)
      result.rows(1).key.content mustBe Text(LastSaoName.key)
      result.rows(1).value.content mustBe HtmlContent(contentWithTestId(multiSaoName1, LastSaoName.testId1))
      result.rows(2).key.content mustBe Text(SaoStartDate.key)
      result.rows(2).value.content mustBe HtmlContent(contentWithTestId(SaoStartDate.content1, SaoStartDate.testId1))
      result.rows(3).key.content mustBe Text(PreviousSaoName.key(multiSaoName1))
      result.rows(3).value.content mustBe HtmlContent(contentWithTestId(multiSaoName2, PreviousSaoName.testId(1)))
      result.rows(4).key.content mustBe Text(SaoStartDate.key)
      result.rows(4).value.content mustBe HtmlContent(contentWithTestId(SaoStartDate.content2, SaoStartDate.testId2))
      result.rows(5).key.content mustBe Text(SaoEndDate.key)
      result.rows(5).value.content mustBe HtmlContent(contentWithTestId(SaoEndDate.content2, SaoEndDate.testId2))
      result.rows(6).key.content mustBe Text(AllAdded.key)
      result.rows(6).value.content mustBe HtmlContent(contentWithTestId(AllAdded.noContent, AllAdded.testId1))
      result.rows(7).key.content mustBe Text(PreviousSaoName.key(multiSaoName2))
      result.rows(7).value.content mustBe HtmlContent(contentWithTestId(multiSaoName3, PreviousSaoName.testId(2)))
      result.rows(8).key.content mustBe Text(SaoStartDate.key)
      result.rows(8).value.content mustBe HtmlContent(contentWithTestId(SaoStartDate.content3, SaoStartDate.testId3))
      result.rows(9).key.content mustBe Text(SaoEndDate.key)
      result.rows(9).value.content mustBe HtmlContent(contentWithTestId(SaoEndDate.content3, SaoEndDate.testId3))
      result.rows(10).key.content mustBe Text(AllAdded.key)
      result.rows(10).value.content mustBe HtmlContent(contentWithTestId(AllAdded.yesContent, AllAdded.testId2))
      result.rows(11).key.content mustBe Text(AdditionalInformation.key)
      result.rows(11).value.content mustBe HtmlContent(AdditionalInformation.providedContent)
    }

    "additional information not provided" in {
      val userAnswers = emptyUserAnswers
        .set(NotificationMoreThanOneSaoPage, true)
        .get
        .set(NotificationAdditionalInformationPage, None)
        .get
        .set(NotificationMultiSaoLastOfficerNamePage, multiSaoName1)
        .get
        .set(NotificationMultiSaoLastOfficerStartDatePage, multiSao1StartDate)
        .get
        .set(NotificationMultiSaoPreviousOfficerNamePage(0), multiSaoName2)
        .get
        .set(NotificationMultiSaoPreviousOfficerStartDatePage(0), multiSao2StartDate)
        .get
        .set(NotificationMultiSaoPreviousOfficerEndDatePage(0), multiSao2EndDate)
        .get
        .set(NotificationMultiSaoAreAllAddedPage(0), false)
        .get
        .set(NotificationMultiSaoPreviousOfficerNamePage(1), multiSaoName3)
        .get
        .set(NotificationMultiSaoPreviousOfficerStartDatePage(1), multiSao3StartDate)
        .get
        .set(NotificationMultiSaoPreviousOfficerEndDatePage(1), multiSao3EndDate)
        .get
        .set(NotificationMultiSaoAreAllAddedPage(1), true)
        .get

      val result = SUT.getSummaryList(userAnswers)

      result.rows.length mustBe 12
      result.rows(0).key.content mustBe Text(SaoChange.key)
      result.rows(0).value.content mustBe HtmlContent(SaoChange.yesContent)
      result.rows(1).key.content mustBe Text(LastSaoName.key)
      result.rows(1).value.content mustBe HtmlContent(contentWithTestId(multiSaoName1, LastSaoName.testId1))
      result.rows(2).key.content mustBe Text(SaoStartDate.key)
      result.rows(2).value.content mustBe HtmlContent(contentWithTestId(SaoStartDate.content1, SaoStartDate.testId1))
      result.rows(3).key.content mustBe Text(PreviousSaoName.key(multiSaoName1))
      result.rows(3).value.content mustBe HtmlContent(contentWithTestId(multiSaoName2, PreviousSaoName.testId(1)))
      result.rows(4).key.content mustBe Text(SaoStartDate.key)
      result.rows(4).value.content mustBe HtmlContent(contentWithTestId(SaoStartDate.content2, SaoStartDate.testId2))
      result.rows(5).key.content mustBe Text(SaoEndDate.key)
      result.rows(5).value.content mustBe HtmlContent(contentWithTestId(SaoEndDate.content2, SaoEndDate.testId2))
      result.rows(6).key.content mustBe Text(AllAdded.key)
      result.rows(6).value.content mustBe HtmlContent(contentWithTestId(AllAdded.noContent, AllAdded.testId1))
      result.rows(7).key.content mustBe Text(PreviousSaoName.key(multiSaoName2))
      result.rows(7).value.content mustBe HtmlContent(contentWithTestId(multiSaoName3, PreviousSaoName.testId(2)))
      result.rows(8).key.content mustBe Text(SaoStartDate.key)
      result.rows(8).value.content mustBe HtmlContent(contentWithTestId(SaoStartDate.content3, SaoStartDate.testId3))
      result.rows(9).key.content mustBe Text(SaoEndDate.key)
      result.rows(9).value.content mustBe HtmlContent(contentWithTestId(SaoEndDate.content3, SaoEndDate.testId3))
      result.rows(10).key.content mustBe Text(AllAdded.key)
      result.rows(10).value.content mustBe HtmlContent(contentWithTestId(AllAdded.yesContent, AllAdded.testId2))
      result.rows(11).key.content mustBe Text(AdditionalInformation.key)
      result.rows(11).value.content mustBe HtmlContent(AdditionalInformation.notProvidedContent)
    }
  }

  "NotificationCheckYourAnswersService.list" - {
    "NotificationSingleSaoOfficerNamePage.row" - {

      "when MoreThanOneSao is No" - {
        "Full Name is answered, must show the Full Name row" in {
          val userAnswers = emptyUserAnswers
            .set(NotificationMoreThanOneSaoPage, false)
            .get
            .set(NotificationSingleSaoOfficerNamePage, singleSaoName)
            .get

          val result = SUT.getSummaryList(userAnswers)

          result.rows(0).key.content mustBe Text(SaoChange.key)
          result.rows(0).value.content mustBe HtmlContent(SaoChange.noContent)
          result.rows(1).key.content mustBe Text(SaoName.key)
          result.rows(1).value.content mustBe HtmlContent(SaoName.content)
        }

        "Full Name is empty, must not show the Full Name row" in {
          val userAnswers = emptyUserAnswers
            .set(NotificationMoreThanOneSaoPage, false)
            .get

          val result = SUT.getSummaryList(userAnswers)

          result.rows(0).key.content mustBe Text(SaoChange.key)
          result.rows(0).value.content mustBe HtmlContent(SaoChange.noContent)
          result.rows.find(row => row.key.content == Text(SaoName.key)) mustBe None
        }
      }

      "when MoreThanOneSao is Yes" - {
        "must not show the Full Name row even if Full Name is answered" in {
          val userAnswers = emptyUserAnswers
            .set(NotificationMoreThanOneSaoPage, true)
            .get
            .set(NotificationSingleSaoOfficerNamePage, singleSaoName)
            .get

          val result = SUT.getSummaryList(userAnswers)

          result.rows.find(row => row.key.content == Text(SaoName.key)) mustBe None
        }
      }
    }

    "NotificationAdditionalInformationPage row" - {

      "when MoreThanOneSao is Yes" - {

        "when there are no answers for NotificationAdditionalInformationPage, must return 'Not provided'" in {
          val userAnswers = emptyUserAnswers
            .set(NotificationMoreThanOneSaoPage, true)
            .get
          val result = SUT.getSummaryList(userAnswers)

          result.rows(1).key.content mustBe Text(AdditionalInformation.key)
          result.rows(1).value.content mustBe HtmlContent(AdditionalInformation.notProvidedContent)
        }

        "when there are answers for NotificationAdditionalInformationPage, must return value" in {
          val userAnswers = emptyUserAnswers
            .set(NotificationMoreThanOneSaoPage, true)
            .get
            .set(NotificationAdditionalInformationPage, Some(additionalInformation))
            .get
          val result = SUT.getSummaryList(userAnswers)

          result.rows(1).key.content mustBe Text(AdditionalInformation.key)
          result.rows(1).value.content mustBe HtmlContent(AdditionalInformation.providedContent)
        }
      }

      "when MoreThanOneSao is No" - {

        "when there are no answers for NotificationAdditionalInformationPage, must return 'Not provided'" in {
          val userAnswers = emptyUserAnswers
            .set(NotificationMoreThanOneSaoPage, false)
            .get
          val result = SUT.getSummaryList(userAnswers)

          result.rows(1).key.content mustBe Text(AdditionalInformation.key)
          result.rows(1).value.content mustBe HtmlContent(AdditionalInformation.notProvidedContent)
        }

        "when there are answers for NotificationAdditionalInformationPage, must return value" in {
          val userAnswers = emptyUserAnswers
            .set(NotificationMoreThanOneSaoPage, false)
            .get
            .set(NotificationAdditionalInformationPage, Some(additionalInformation))
            .get
          val result = SUT.getSummaryList(userAnswers)

          result.rows(1).key.content mustBe Text(AdditionalInformation.key)
          result.rows(1).value.content mustBe HtmlContent(AdditionalInformation.providedContent)
        }
      }
    }
  }
}

object NotificationCheckYourAnswersServiceSpec {
  val singleSaoName         = "Firstname Lastname"
  val multiSaoName1         = "Firstname Lastname II"
  val multiSaoName2         = "Firstname Lastname III"
  val multiSaoName3         = "Firstname Lastname IV"
  val multiSao1StartDate    = LocalDate.of(2024, 6, 1)
  val multiSao2StartDate    = LocalDate.of(2024, 6, 2)
  val multiSao3StartDate    = LocalDate.of(2024, 6, 3)
  val multiSao1EndDate      = LocalDate.of(2024, 6, 4)
  val multiSao2EndDate      = LocalDate.of(2024, 6, 5)
  val multiSao3EndDate      = LocalDate.of(2024, 6, 6)
  val additionalInformation = "Additional information is not that remarkable."

  object SaoChange {
    val key        = "Did the SAO change during the financial year?"
    val yesContent = """<span data-test-id="sao-change-value">Yes</span>"""
    val noContent  = """<span data-test-id="sao-change-value">No</span>"""
  }

  object SaoName {
    val key     = "Senior Accounting Officer"
    val content = s"""<span data-test-id="sao-name-value">$singleSaoName</span>"""
  }

  object AdditionalInformation {
    val key                = "Additional information"
    val providedContent    = s"""<span data-test-id="additional-information-value">$additionalInformation</span>"""
    val notProvidedContent = """<span data-test-id="additional-information-value">Not provided</span>"""
  }

  object LastSaoName {
    val key     = "SAO at the end of the financial year"
    val testId1 = "final-sao-name"
  }

  object SaoStartDate {
    val key      = "Start date"
    val content1 = "1 June 2024"
    val content2 = "2 June 2024"
    val content3 = "3 June 2024"
    val testId1  = "final-sao-start-date"
    val testId2  = "previous-sao-start-date-1"
    val testId3  = "previous-sao-start-date-2"
  }

  object SaoEndDate {
    val key      = "End date"
    val content1 = "4 June 2024"
    val content2 = "5 June 2024"
    val content3 = "6 June 2024"
    val testId1  = "final-sao-end-date"
    val testId2  = "previous-sao-end-date-1"
    val testId3  = "previous-sao-end-date-2"
  }

  object AllAdded {
    val key        = "Have you added all the SAOs for this notification?"
    val yesContent = "Yes"
    val noContent  = "No"
    val testId1    = "sao-are-all-added-1"
    val testId2    = "sao-are-all-added-2"
  }

  object PreviousSaoName {
    def key(saoName: String): String = s"SAO before $saoName"
    def testId(index: Int): String   = s"previous-sao-name-$index"
  }

  def contentWithTestId(content: String, testId: String): String = {
    s"""<span data-test-id="$testId">$content</span>"""
  }
}
