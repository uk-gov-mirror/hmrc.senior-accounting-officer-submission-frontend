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

package controllers

import base.SpecBase
import config.AppConfig
import controllers.DownloadTemplateControllerSpec.*
import org.apache.pekko.stream.Materializer
import org.apache.pekko.stream.scaladsl.StreamConverters
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import play.api.http.Status
import play.api.mvc.{AnyContentAsEmpty, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.http.InternalServerException

import scala.concurrent.Future

import java.io.InputStream

class DownloadTemplateControllerSpec extends SpecBase {

  given request: FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest(GET, routes.DownloadTemplateController.downloadFile().url)

  "GET must " - {
    "return a file with correct name,type and headers" in {

      val app = applicationBuilder(userAnswers = None).build()
      running(app) {
        val result = route(app, request).value

        status(result) mustBe Status.OK

        val contentDisposition = header("Content-Disposition", result)

        contentDisposition mustBe Some(
          "attachment; filename=Senior Accounting Officer notification and certificate submission template.xlsx"
        )
        contentType(result) mustBe Some("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
      }

    }

    "the returned Excel template must be saved with active cell in A1 or unspecified" in {
      val app = applicationBuilder(userAnswers = None).build()

      running(app) {

        given Materializer = app.injector.instanceOf

        val result = route(app, request).value

        status(result) mustBe Status.OK

        val workbook          = contentAsXlsxWorkBook(result)
        val activeCellAddress = workbook.getSheetAt(workbook.getActiveSheetIndex).getActiveCell

        withClue("""Excel must open with the A1 cell selected for accessibility reasons
            |(This can be achieved either by explicit configuring it to A1 or left unset)
            |e.g.
            |Steps to set the workbook to open on this cell on Mac
            |Open Microsoft Excel.
            |Select the A1 cell.
            |Save your workbook.
            |""".stripMargin) {
          Option(activeCellAddress).fold("A1")(_.formatAsString()) mustBe "A1"
        }
      }
    }

    "the returned Excel template must not have author or last save by information" in {
      val app = applicationBuilder(userAnswers = None).build()

      running(app) {

        given Materializer = app.injector.instanceOf

        val result = route(app, request).value

        status(result) mustBe Status.OK

        val workbook  = contentAsXlsxWorkBook(result)
        val coreProps = workbook.getProperties.getCoreProperties

        withClue("""Personal information in the xlsx template must be removed.
            |e.g.
            |Steps to Remove "Author" and "Last Saved By" on Mac
            |Open Microsoft Excel.
            |Click Excel in the top menu bar and select Preferences (or go to Tools > Protect Document).
            |Click on the Security section. Check the box for "Remove personal information from this file on save".
            |Close the preferences window and save your workbook.
            |""".stripMargin) {
          Option(coreProps.getCreator) mustBe None
          Option(coreProps.getLastModifiedByUser) mustBe None
        }
      }

    }

    "throw an InternalServerException if template file is unavailable" in {

      val app = applicationBuilder(userAnswers = None).build()
      running(app) {
        AppConfig.setValue("templateFile", "nonsense/file/path")
        val result = route(app, request).value

        intercept[InternalServerException] {
          status(result)
        }
      }
    }
  }
}

object DownloadTemplateControllerSpec {

  def contentAsXlsxWorkBook(result: Future[Result])(using Materializer): XSSFWorkbook = {
    val templateAsInputStream: InputStream =
      await(result).body.dataStream.runWith(StreamConverters.asInputStream())

    XSSFWorkbook(templateAsInputStream)
  }

}
