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
import play.api.http.Status
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.http.InternalServerException

class NotificationTemplateDownloadControllerSpec extends SpecBase {

  given request: FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest(GET, routes.DownloadNotificationTemplateController.downloadFile().url)

  "GET must " - {
    "return a file with correct name,type and headers" in {

      val app = applicationBuilder(userAnswers = None).build()
      running(app) {
        val result = route(app, request).value

        status(result) mustBe Status.OK

        val contentDisposition = header("Content-Disposition", result)

        contentDisposition mustBe Some("attachment; filename=Submission template v15.xlsx")
        contentType(result) mustBe Some("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
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
