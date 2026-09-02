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

package viewmodels.checkAnswers.notification

import base.SpecBase
import controllers.notification.routes as notificationRoutes
import models.CheckMode
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import pages.notification.NotificationMultiSaoPreviousOfficerEndDatePage
import play.api.i18n.{Messages, MessagesApi}
import uk.gov.hmrc.govukfrontend.views.Implicits.RichString

import java.time.LocalDate

class NotificationMultiSaoPreviousOfficerEndDateSummarySpec extends SpecBase with GuiceOneAppPerSuite {
  given Messages = app.injector.instanceOf[MessagesApi].preferred(Seq.empty)

  "NotificationMultiSaoPreviousOfficerEndDateSummary.row" - {

    "when there is no answer for NotificationMultiSaoPreviousOfficerEndDatePage" - {
      "must return None" in {
        def SUT = NotificationMultiSaoPreviousOfficerEndDateSummary.row(emptyUserAnswers, 0)

        SUT mustBe None
      }
    }

    "when there is a user answer for NotificationMultiSaoPreviousOfficerEndDatePage" - {
      def testUserAnswers(answer: LocalDate) =
        emptyUserAnswers.set(NotificationMultiSaoPreviousOfficerEndDatePage(0), answer).get

      def SUT(answer: LocalDate = LocalDate.now) =
        NotificationMultiSaoPreviousOfficerEndDateSummary.row(testUserAnswers(answer), 0).get

      "must have expected key" in {
        SUT().key mustBe "NotificationMultiSaoPreviousOfficerEndDate".toKey
      }

      "expected value" - {
        "must show '1 January 2000' when user answers is 1st Jan 2000" in {
          SUT(answer = LocalDate.of(2000, 1, 1)).value.content mustBe "1 January 2000".toText
        }
      }

      "expected action" - {
        def actions = SUT().actions

        "must only have one action" in {
          withClue("must be 1 action\n") {
            actions.size mustBe 1
          }
          withClue("must be 1 item in the action\n") {
            actions.head.items.size mustBe 1
          }
        }

        def action = actions.head.items.head

        "must have expected text" in {
          action.content mustBe "Change".toText
        }

        "must have expected url" in {
          action.href mustBe notificationRoutes.NotificationMultiSaoPreviousOfficerEndDateController
            .onPageLoad(CheckMode)
            .url
        }

        "must have expected hidden text" in {
          action.visuallyHiddenText.get mustBe "NotificationMultiSaoPreviousOfficerEndDate"
        }
      }
    }
  }

}
