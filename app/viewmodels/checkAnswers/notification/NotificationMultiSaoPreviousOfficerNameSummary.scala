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
import pages.notification.NotificationMultiSaoPreviousOfficerNamePage
import play.api.i18n.Messages
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import viewmodels.converters.*
import viewmodels.govuk.summarylist.*

object NotificationMultiSaoPreviousOfficerNameSummary {

  def row(answers: UserAnswers, saoIndex: Int)(using messages: Messages): Option[SummaryListRow] =
    answers.get(NotificationMultiSaoPreviousOfficerNamePage(saoIndex)).map { answer =>
      SummaryListRowViewModel(
        key = messages("notificationMultiSaoPreviousOfficerName.checkYourAnswersLabel").toKey,
        value = ValueViewModel(answer.toText),
        actions = Seq(
          ActionItemViewModel(
            messages("site.change").toText,
            notificationRoutes.NotificationMultiSaoPreviousOfficerNameController.onPageLoad(CheckMode, saoIndex).url
          )
            .withVisuallyHiddenText(messages("notificationMultiSaoPreviousOfficerName.change.hidden"))
        )
      )
    }
}
