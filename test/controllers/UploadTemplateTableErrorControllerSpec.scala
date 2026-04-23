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

package controllers

import base.SpecBase
import models.upload.*
import pages.UploadTemplateTablePage
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import views.html.UploadTemplateTableErrorView

class UploadTemplateTableErrorControllerSpec extends SpecBase {

  private val tableData = UploadTemplateTableData(
    rows = Seq.empty,
    errors = Seq(TemplateParseError(9, Some("Company UTR"), "missing_required_value", "UTR is required"))
  )

  private val populatedAnswers = emptyUserAnswers.set(UploadTemplateTablePage, tableData).success.value

  "UploadTemplateTableError Controller" - {

    "must return OK and the correct view for a GET" in {
      val application = applicationBuilder(userAnswers = Some(populatedAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, routes.UploadTemplateTableErrorController.onPageLoad().url)

        val result = route(application, request).value

        val view = application.injector.instanceOf[UploadTemplateTableErrorView]

        status(result) mustEqual OK
        contentAsString(result) mustEqual view(tableData)(using request, messages(application)).toString
      }
    }

    "must redirect to upload form for a POST" in {
      val application = applicationBuilder(userAnswers = Some(populatedAnswers)).build()

      running(application) {
        val request = FakeRequest(POST, routes.UploadTemplateTableErrorController.onSubmit().url)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.NotificationUploadFormController.onPageLoad().url
      }
    }

    "must redirect to Journey Recovery for GET when table data is missing" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, routes.UploadTemplateTableErrorController.onPageLoad().url)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to Journey Recovery for POST when table data is missing" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val request = FakeRequest(POST, routes.UploadTemplateTableErrorController.onSubmit().url)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.JourneyRecoveryController.onPageLoad().url
      }
    }
  }
}
