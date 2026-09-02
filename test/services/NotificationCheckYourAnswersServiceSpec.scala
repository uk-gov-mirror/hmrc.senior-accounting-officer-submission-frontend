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

class NotificationCheckYourAnswersServiceSpec extends SpecBase with GuiceOneAppPerSuite {

  "NotificationCheckYourAnswersService.list" - {

    def SUT: NotificationCheckYourAnswersService = app.injector.instanceOf[NotificationCheckYourAnswersService]

    given Messages = app.injector.instanceOf[MessagesApi].preferred(FakeRequest())

    val testFullName              = "testName"
    val testAdditionalInformation = "testValue"

    "NotificationSingleSaoOfficerNamePage.row" - {

      "when MoreThanOneSao is No" - {
        "Full Name is answered, must show the Full Name row" in {
          val userAnswers = emptyUserAnswers
            .set(NotificationMoreThanOneSaoPage, false)
            .get
            .set(NotificationSingleSaoOfficerNamePage, testFullName)
            .get

          val result = SUT.getSummaryList(userAnswers)

          result.rows.head.key.content mustBe Text("Senior Accounting Officer")
          result.rows.head.value.content mustBe HtmlContent(
            s"""<span data-test-id="sao-name-value">$testFullName</span>"""
          )
        }

        "Full Name is empty, must not show the Full Name row" in {
          val userAnswers = emptyUserAnswers
            .set(NotificationMoreThanOneSaoPage, false)
            .get

          val result = SUT.getSummaryList(userAnswers)

          result.rows.find(row => row.key.content == Text("Senior Accounting Officer")) mustBe None
        }
      }

      "when MoreThanOneSao is Yes" - {
        "must not show the Full Name row even if Full Name is answered" in {
          val userAnswers = emptyUserAnswers
            .set(NotificationMoreThanOneSaoPage, true)
            .get
            .set(NotificationSingleSaoOfficerNamePage, testFullName)
            .get

          val result = SUT.getSummaryList(userAnswers)

          result.rows.find(row => row.key.content == Text("Senior Accounting Officer")) mustBe None

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

          result.rows.head.value.content mustBe HtmlContent(
            s"""<span data-test-id="additional-information-value">Not provided</span>"""
          )
        }

        "when there are answers for NotificationAdditionalInformationPage, must return value" in {
          val userAnswers = emptyUserAnswers
            .set(NotificationMoreThanOneSaoPage, true)
            .get
            .set(NotificationAdditionalInformationPage, Some(testAdditionalInformation))
            .get
          val result = SUT.getSummaryList(userAnswers)

          result.rows.head.value.content mustBe HtmlContent(
            s"""<span data-test-id="additional-information-value">$testAdditionalInformation</span>"""
          )
        }
      }

      "when MoreThanOneSao is No" - {

        "when there are no answers for NotificationAdditionalInformationPage, must return 'Not provided'" in {
          val userAnswers = emptyUserAnswers
            .set(NotificationMoreThanOneSaoPage, false)
            .get
          val result = SUT.getSummaryList(userAnswers)

          result.rows.head.value.content mustBe HtmlContent(
            s"""<span data-test-id="additional-information-value">Not provided</span>"""
          )
        }

        "when there are answers for NotificationAdditionalInformationPage, must return value" in {
          val userAnswers = emptyUserAnswers
            .set(NotificationMoreThanOneSaoPage, false)
            .get
            .set(NotificationAdditionalInformationPage, Some(testAdditionalInformation))
            .get
          val result = SUT.getSummaryList(userAnswers)

          result.rows.head.value.content mustBe HtmlContent(
            s"""<span data-test-id="additional-information-value">$testAdditionalInformation</span>"""
          )
        }
      }

    }
  }
}
