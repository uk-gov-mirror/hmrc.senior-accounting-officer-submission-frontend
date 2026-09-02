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

import controllers.notification.routes as notificationRoutes
import models.{CheckMode, UserAnswers}
import pages.notification.NotificationMultiSaoPreviousOfficerEndDatePage
import play.api.i18n.{Lang, Messages}
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import utils.DateTimeFormats.dateTimeFormat
import viewmodels.converters.*
import viewmodels.govuk.summarylist.*

object NotificationMultiSaoPreviousOfficerEndDateSummary {

  def row(answers: UserAnswers, saoIndex: Int)(using messages: Messages): Option[SummaryListRow] =
    answers.get(NotificationMultiSaoPreviousOfficerEndDatePage(saoIndex)).map { answer =>
      given Lang = messages.lang
      SummaryListRowViewModel(
        key = messages("notificationMultiSaoPreviousOfficerEndDate.checkYourAnswersLabel").toKey,
        value = ValueViewModel(answer.format(dateTimeFormat()).toText),
        actions = Seq(
          ActionItemViewModel(
            messages("site.change").toText,
            notificationRoutes.NotificationMultiSaoPreviousOfficerEndDateController.onPageLoad(CheckMode).url
          )
            .withVisuallyHiddenText(messages("notificationMultiSaoPreviousOfficerEndDate.change.hidden"))
        )
      )
    }
}
