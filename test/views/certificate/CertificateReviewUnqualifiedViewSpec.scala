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
import base.ViewSpecBase.excludeHelpLinkAndErrorMessageParagraphsSelector
import controllers.certificate.routes as certificateRoutes
import models.UnqualifiedCompany
import models.upload.{CompanyStatus, CompanyType}
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import views.html.certificate.CertificateReviewUnqualifiedView

import java.time.LocalDate

import CertificateReviewUnqualifiedViewSpec.*

class CertificateReviewUnqualifiedViewSpec extends ViewSpecBase[CertificateReviewUnqualifiedView] {
  private def generateView(
      saoName: String,
      unqualifiedCompanies: Seq[UnqualifiedCompany],
      companyCount: Int
  ): Document = Jsoup.parse(SUT(saoName, unqualifiedCompanies, companyCount).toString)

  "CertificateReviewUnqualifiedView" - {
    "will populate the table correctly when data is present" - {

      val doc: Document = generateView(saoName, unqualifiedCompanies, 10)

      doc.createTestsWithStandardPageElements(
        pageTitle = pageTitle,
        pageHeading = pageHeading,
        showBackLink = true,
        showIsThisPageNotWorkingProperlyLink = true,
        hasError = false
      )

      doc.createTestsWithCaption(pageCaption)

      doc.createTestsWithOrWithoutError(hasError = false)

      doc.createTestsWithParagraphs(paragraphs)

      doc.createTestsWithSubmissionButton(
        action = certificateRoutes.CertificateReviewUnqualifiedController.onSubmit(),
        buttonText = "Continue"
      )

      doc
        .select(linkLocator)
        .get(0)
        .createTestWithLink(
          linkTestDescription = "upload updated submission template",
          linkText = linkText,
          destinationUrl = certificateRoutes.CertificateUploadFormController.onPageLoad().url,
          isExternalLink = false
        )

      doc.createTestsWithUnqualifiedCompanyDescriptionList(unqualifiedCompanies)

    }

    "will populate the table correctly when no data is present" - {

      val doc: Document = generateView("", Seq.empty, 0)

      doc.createTestsWithStandardPageElements(
        pageTitle = pageTitle,
        pageHeading = pageHeading,
        showBackLink = true,
        showIsThisPageNotWorkingProperlyLink = true,
        hasError = false
      )

      doc.createTestsWithCaption(pageCaption)

      doc.createTestsWithOrWithoutError(hasError = false)

      doc.createTestsWithParagraphs(paragraphsWithNoData)

      doc.createTestsWithSubmissionButton(
        action = certificateRoutes.CertificateReviewUnqualifiedController.onSubmit(),
        buttonText = "Continue"
      )

      doc
        .select(linkLocator)
        .get(0)
        .createTestWithLink(
          linkTestDescription = "update uploaded submission template",
          linkText = linkText,
          destinationUrl = certificateRoutes.CertificateUploadFormController.onPageLoad().url,
          isExternalLink = false
        )

      doc.createTestsWithUnqualifiedCompanyDescriptionList(Seq())

    }

    "will use singular wording when one company is present" - {
      val doc = generateView(saoName, unqualifiedCompanies.take(1), 1)

      doc.createTestsWithParagraphs(paragraphsWithOneCompany)

    }

    "The content of SAO's name is properly escaped" in {
      val unescapedText = "<script>alert(123)</script>"
      val doc: Document = generateView(unescapedText, unqualifiedCompanies, 2)

      doc.select(excludeHelpLinkAndErrorMessageParagraphsSelector).eachText().toArray.toSeq.mkString(",") must include(
        unescapedText
      )
    }
  }
  extension (doc: Document) {
    def createTestsWithUnqualifiedCompanyDescriptionList(unqualifiedCompanies: Seq[UnqualifiedCompany]): Unit = {
      val expectedCountOfDescriptionLists = unqualifiedCompanies.size

      "must be one description list per unqualified company" in {
        val descriptionLists = doc.select("dl.govuk-summary-list")
        descriptionLists.size() mustBe expectedCountOfDescriptionLists
      }

      "must be five description terms per unqualified company with expected text" in {
        val descriptionTerms = doc.select("div.govuk-summary-list__row > dt.govuk-summary-list__key")

        descriptionTerms.size() mustBe expectedCountOfDescriptionLists * 6

        for i <- unqualifiedCompanies.indices do {
          descriptionTerms.get(i * 6).text() mustBe companyNameDescriptionTermText
          descriptionTerms.get(i * 6 + 1).text() mustBe utrDescriptionTermText
          descriptionTerms.get(i * 6 + 2).text() mustBe crnDescriptionTermText
          descriptionTerms.get(i * 6 + 3).text() mustBe typeDescriptionTermText
          descriptionTerms.get(i * 6 + 4).text() mustBe statusDescriptionTermText
          descriptionTerms.get(i * 6 + 5).text() mustBe financialYearEndDateDescriptionTermText
        }
      }

      "must be five description details per unqualified company with expected text" in {
        val descriptionDetails = doc.select("div.govuk-summary-list__row > dd.govuk-summary-list__value")
        descriptionDetails.size() mustBe expectedCountOfDescriptionLists * 6

        for i <- unqualifiedCompanies.indices do {
          descriptionDetails.get(i * 6).text() mustBe unqualifiedCompanies(i).name
          descriptionDetails.get(i * 6 + 1).text() mustBe unqualifiedCompanies(i).utr
          descriptionDetails.get(i * 6 + 2).text() mustBe unqualifiedCompanies(i).crn.fold("")(identity)
          descriptionDetails.get(i * 6 + 3).text() mustBe unqualifiedCompanies(i).companyType.toString
          descriptionDetails.get(i * 6 + 4).text() mustBe unqualifiedCompanies(i).companyStatus.toString
          descriptionDetails.get(i * 6 + 5).text() mustBe testDateAsString(unqualifiedCompanies(i).financialYearEndDate)
        }
      }
    }
  }
}

object CertificateReviewUnqualifiedViewSpec {
  val pageHeading             = "Review the companies with an unqualified certificate"
  val pageCaption             = "Submit a certificate"
  val pageTitle: String       = "Review the companies with an unqualified certificate - Submit a certificate"
  val linkLocator             = ".govuk-body:nth-of-type(2) .govuk-link"
  val linkText                = "upload an updated submission template"
  val paragraphs: Seq[String] = Seq(
    "This list is from the certificate details in your submission template. There were 10 companies the SAO was responsible for in a previous financial year.",
    "If any company’s listed are missing or incorrect, upload an updated submission template before continuing.",
    "In accordance with Paragraph 2, Schedule 46 of the Finance Act 2009, I example sao name, the Senior Accounting Officer hereby certify that 2 companies had appropriate tax accounting arrangements throughout the year."
  )

  val paragraphsWithNoData: Seq[String] = Seq(
    "This list is from the certificate details in your submission template. There were 0 companies the SAO was responsible for in a previous financial year.",
    "If any company’s listed are missing or incorrect, upload an updated submission template before continuing.",
    "In accordance with Paragraph 2, Schedule 46 of the Finance Act 2009, I , the Senior Accounting Officer hereby certify that 0 companies had appropriate tax accounting arrangements throughout the year."
  )

  val paragraphsWithOneCompany: Seq[String] = Seq(
    "This list is from the certificate details in your submission template. There was 1 company the SAO was responsible for in a previous financial year.",
    "If any company’s listed are missing or incorrect, upload an updated submission template before continuing.",
    "In accordance with Paragraph 2, Schedule 46 of the Finance Act 2009, I example sao name, the Senior Accounting Officer hereby certify that 1 company had appropriate tax accounting arrangements throughout the year."
  )

  private val testDate1 = LocalDate.of(2026, 1, 1)
  private val testDate2 = LocalDate.of(2027, 3, 4)

  def testDateAsString(localDate: LocalDate): String =
    Map(
      testDate1 -> "1 January 2026",
      testDate2 -> "4 March 2027"
    ).get(localDate).fold(throw RuntimeException("Unknown test date"))(identity)

  val unqualifiedCompanies: Seq[UnqualifiedCompany] = Seq(
    UnqualifiedCompany(
      name = "example company name",
      utr = "example utr",
      crn = Some("example crn"),
      companyType = CompanyType.LTD,
      companyStatus = CompanyStatus.Active,
      financialYearEndDate = testDate1
    ),
    UnqualifiedCompany(
      name = "example company name 2",
      utr = "example utr 2",
      crn = Some("example crn 2"),
      companyType = CompanyType.PLC,
      companyStatus = CompanyStatus.Dormant,
      financialYearEndDate = testDate2
    )
  )

  val companyNameDescriptionTermText          = "Company name"
  val utrDescriptionTermText                  = "UTR"
  val crnDescriptionTermText                  = "CRN"
  val typeDescriptionTermText                 = "Type"
  val statusDescriptionTermText               = "Status"
  val financialYearEndDateDescriptionTermText = "Financial year end"

  val saoName = "example sao name"

}
