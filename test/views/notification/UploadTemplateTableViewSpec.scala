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

import scala.jdk.CollectionConverters.*

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

    doc.createTestsWithParagraphs(paragraphs = Seq(paragraphLine1, paragraphLine2, paragraphLine3))

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

    "must render the SAO name, company count and upload link" in {
      doc.text() must include(saoName)
      doc.text() must include("There were 1 companies")
      val uploadLink =
        doc.select("a.govuk-link").asScala.find(_.text() == "upload an updated submission template").value
      uploadLink.attr("href") must endWith("/notification/upload")
    }

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

  val pageHeading            = "Review the companies in your notification"
  val pageTitle              = "Review the companies in your notification"
  val saoName                = "Jane Smith"
  val paragraphLine1: String =
    "This list is taken from the notification details in the submission template you uploaded."
  val paragraphLine2: String =
    s"There were ${tableData.rows.size} companies $saoName was responsible for during the financial year."
  val paragraphLine3: String = "If this is not correct, upload an updated submission template before continuing."
  val pageCaption            = "Submit a notification"

}
