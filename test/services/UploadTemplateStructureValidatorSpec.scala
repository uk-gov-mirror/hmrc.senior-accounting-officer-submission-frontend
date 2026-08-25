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

import base.SpecBase
import services.csvparser.UploadTemplateCsvSchema.*
import services.csvparser.UploadTemplateStructureValidator

class UploadTemplateStructureValidatorSpec extends SpecBase {

  private val validator = new UploadTemplateStructureValidator()

  "validateSectionRow" - {

    "must return a missing_section_row error when no section row exists" in {
      val errors = validator.validateSectionRow(None)

      errors mustBe Seq(
        models.upload.TemplateParseError(
          line = SectionLineNumber,
          column = None,
          error = TemplateError.InvalidTemplateError
        )
      )
    }

    "must return both invalid_section_row errors when both section cells are wrong" in {
      val row    = Vector("", "wrong", "", "", "", "", "also-wrong")
      val errors = validator.validateSectionRow(Some(row))

      errors.map(_.error) mustBe Seq(TemplateError.InvalidTemplateError, TemplateError.InvalidTemplateError)
      errors.map(_.line).distinct mustBe Seq(SectionLineNumber)
    }
  }

  "validateHeaderRow" - {

    "must return a missing_header_row error when no header row exists" in {
      val errors = validator.validateHeaderRow(None)

      errors mustBe Seq(
        models.upload.TemplateParseError(
          line = HeaderLineNumber,
          column = None,
          error = TemplateError.InvalidTemplateError
        )
      )
    }

    "must return unexpected_header_columns when non-empty extra columns are present" in {
      val headerWithExtra = ExpectedHeaders.toVector :+ "extra-header"

      val errors = validator.validateHeaderRow(Some(headerWithExtra))

      errors.exists(_.error == TemplateError.InvalidTemplateError) mustBe true
      errors.map(_.line).distinct mustBe Seq(HeaderLineNumber)
    }
  }
}
