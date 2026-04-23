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

package views

import base.ViewSpecBase
import models.upload.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import views.UploadTemplateTableErrorViewSpec.*
import views.html.UploadTemplateTableErrorView

class UploadTemplateTableErrorViewSpec extends ViewSpecBase[UploadTemplateTableErrorView] {

  private def generateView(): Document = Jsoup.parse(SUT(tableData).toString)

  "UploadTemplateTableErrorView" - {
    val doc: Document = generateView()

    doc.createTestsWithStandardPageElements(
      pageTitle = pageTitle,
      pageHeading = pageHeading,
      showBackLink = true,
      showIsThisPageNotWorkingProperlyLink = true,
      hasError = false
    )

    "must render error table columns and content" in {
      val headings = doc.select("th.govuk-table__header").eachText()
      headings must contain allOf ("Line", "Column", "Code", "Message")
      doc.select("tbody.govuk-table__body tr").size() must be >= 1
    }

    "must render upload another file button" in {
      doc.select("#continue").size() mustBe 1
    }
  }
}

object UploadTemplateTableErrorViewSpec {
  val pageHeading = "Fix the errors in your file"
  val pageTitle   = "There is a problem with your file"

  val tableData: UploadTemplateTableData = UploadTemplateTableData(
    rows = Seq.empty,
    errors = Seq(
      TemplateParseError(
        line = 9,
        column = Some("Company UTR"),
        code = "missing_required_value",
        message = "Line 9 Company UTR is required."
      )
    )
  )
}
