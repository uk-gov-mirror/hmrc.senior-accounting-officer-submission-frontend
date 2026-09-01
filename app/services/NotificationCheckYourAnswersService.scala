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

package services

import models.UserAnswers
import pages.notification.NotificationMoreThanOneSaoPage
import play.api.i18n.Messages
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList
import viewmodels.checkAnswers.notification.{
  NotificationAdditionalInformationSummary,
  NotificationSingleSaoOfficerNameSummary
}
import viewmodels.checkAnswers.notification.*
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import scala.annotation.tailrec
import pages.notification.NotificationMultiSaoAreAllAddedPage

class NotificationCheckYourAnswersService {
  def getSummaryList(userAnswers: UserAnswers)(using Messages): SummaryList = {
    SummaryList(rows =
      (
        NotificationMoreThanOneSaoSummary.row(userAnswers)
          +: (userAnswers.get(NotificationMoreThanOneSaoPage) match {
            case Some(false) => rowsForSingleSao(userAnswers)
            case Some(true)  => rowsForMultipleSaos(userAnswers)
            case _           => ???
          })
          :+ Some(NotificationAdditionalInformationSummary.row(userAnswers))
      ).flatten
    )
  }

  private def rowsForSingleSao(userAnswers: UserAnswers)(using Messages): Seq[Option[SummaryListRow]] = {
    Seq(NotificationSingleSaoOfficerNameSummary.row(userAnswers))
  }

  @tailrec
  private def rowsForMultipleSaos(
      userAnswers: UserAnswers,
      index: Int = 0,
      result: Seq[Option[SummaryListRow]] = Nil
  )(using Messages): Seq[Option[SummaryListRow]] = {
    userAnswers.get(NotificationMultiSaoAreAllAddedPage(index)) match {
      case Some(true) =>
        NotificationMultiSaoLastOfficerNameSummary.row(userAnswers)
          +: NotificationMultiSaoLastOfficerStartDateSummary.row(userAnswers)
          +: (result ++ rowsForOneOfMultipleSaos(userAnswers, index))
      case Some(false) =>
        rowsForMultipleSaos(
          userAnswers,
          index + 1,
          result ++ rowsForOneOfMultipleSaos(userAnswers, index)
        )
      case None => Nil
    }
  }

  private def rowsForOneOfMultipleSaos(userAnswers: UserAnswers, index: Int)(using
      Messages
  ): Seq[Option[SummaryListRow]] = {
    Seq(
      NotificationMultiSaoPreviousOfficerNameSummary.row(
        userAnswers,
        index
      ),
      NotificationMultiSaoPreviousOfficerStartDateSummary.row(
        userAnswers,
        index
      ),
      NotificationMultiSaoPreviousOfficerEndDateSummary.row(
        userAnswers,
        index
      ),
      NotificationMultiSaoAreAllAddedSummary.row(
        userAnswers,
        index
      )
    )
  }
}
