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

package views.notification

import base.ViewSpecBase
import controllers.notification.routes as notificationRoutes
import models.upload.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import views.html.notification.UploadTemplateTableView

import java.time.LocalDate

import UploadTemplateTableViewSpec.*

class UploadTemplateTableViewSpec extends ViewSpecBase[UploadTemplateTableView] {

  private def generateView(): Document = Jsoup.parse(SUT(tableData, saoName).toString)

  "UploadTemplateTableView" - {
    val doc: Document = generateView()

    doc.createTestsWithStandardPageElements(
      pageTitle = pageTitle,
      pageHeading = pageHeading,
      showBackLink = true,
      showIsThisPageNotWorkingProperlyLink = true,
      hasError = false
    )

    doc.createTestsWithParagraphs(paragraphs)

    doc.createTestsWithCaption(pageCaption)

    "must render parsed data table" in {
      val tableHeaders = doc.select("th.govuk-table__header")
      tableHeaders.size() must be >= 6

      val tableRows = doc.select("tbody.govuk-table__body tr")
      tableRows.size() must be >= 1
    }

    "must render company table headings without pagination controls" in {
      val headings = doc.select("th.govuk-table__header").eachText()
      headings must contain allOf ("Company name", "UTR", "CRN", "Type", "Status", "Financial year end")
      doc.select(".govuk-pagination").size() mustBe 0
    }

    doc.createTestsWithSubmissionButton(
      action = notificationRoutes.UploadTemplateTableController.onSubmit(),
      buttonText = "Continue"
    )

    doc.getMainContent
      .select("a.govuk-link")
      .get(0)
      .createTestWithLink(
        linkText,
        notificationRoutes.NotificationUploadFormController.onPageLoad().url
      )
  }
}

object UploadTemplateTableViewSpec {

  val tableData: UploadTemplateTableData = UploadTemplateTableData(
    rows = Seq(
      ParsedSubmissionRow(
        notification = NotificationFields(
          companyName = "Acme Plc",
          companyUtr = CompanyUtr("0123456789"),
          companyCrn = Some(CompanyCrn("12345678")),
          companyType = CompanyType.PLC,
          companyStatus = CompanyStatus.Active,
          financialYearEndDate = LocalDate.of(2025, 12, 31)
        ),
        certificate = Some(
          CertificateFields(
            corporationTax = true,
            valueAddedTax = false,
            paye = false,
            insurancePremiumTax = false,
            stampDutyLandTax = false,
            stampDutyReserveTax = false,
            petroleumRevenueTax = false,
            customsDuties = false,
            exciseDuties = false,
            bankLevy = false,
            certificateType = Some(CertificateType.Qualified),
            qualificationStatement = Some("Example")
          )
        )
      )
    ),
    errors = Seq.empty
  )

  val pageHeading = "Review the companies in your notification"
  val pageTitle   = "Review the companies in your notification - Submit a notification"
  val saoName     = "Jane Smith"
  val paragraphs  = Seq(
    s"This list is from your submission template. It shows ${tableData.rows.size} companies $saoName was responsible for in the financial year.",
    "If any companies details are missing or incorrect, upload an updated submission template before continuing."
  )
  val pageCaption = "Submit a notification"
  val linkText    = "upload an updated submission template"
}
