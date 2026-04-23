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

package services.CSVParser

import models.upload.TemplateParseError
import services.CSVParser.UploadTemplateCsvSchema.*

import javax.inject.Inject

class UploadTemplateStructureValidator @Inject() () {

  def validateSectionRow(rowOpt: Option[CsvRow], templateFileErrorMessage: String): Seq[TemplateParseError] =
    rowOpt match {
      case None =>
        Seq(
          TemplateParseError(
            line = SectionLineNumber,
            column = None,
            code = "missing_section_row",
            message = templateFileErrorMessage
          )
        )
      case Some(row) =>
        val notificationSection = cellValue(row, NotificationSectionIndex)
        val certificateSection  = cellValue(row, CertificateSectionIndex)

        Seq(
          Option.when(notificationSection != NotificationSectionExpected)(
            TemplateParseError(
              line = SectionLineNumber,
              column = Some("Notification"),
              code = "invalid_section_row",
              message = templateFileErrorMessage
            )
          ),
          Option.when(certificateSection != CertificateSectionExpected)(
            TemplateParseError(
              line = SectionLineNumber,
              column = Some("Certificate"),
              code = "invalid_section_row",
              message = templateFileErrorMessage
            )
          )
        ).flatten
    }

  def validateHeaderRow(rowOpt: Option[CsvRow], templateFileErrorMessage: String): Seq[TemplateParseError] =
    rowOpt match {
      case None =>
        Seq(
          TemplateParseError(
            line = HeaderLineNumber,
            column = None,
            code = "missing_header_row",
            message = templateFileErrorMessage
          )
        )
      case Some(row) =>
        val extraColumnError = Option.when(row.drop(ExpectedHeaders.length).exists(_.trim.nonEmpty))(
          TemplateParseError(
            line = HeaderLineNumber,
            column = None,
            code = "unexpected_header_columns",
            message = templateFileErrorMessage
          )
        )

        val headerErrors = ExpectedHeaders.zipWithIndex.collect {
          case (expectedHeader, idx) if cellValue(row, idx) != expectedHeader =>
            TemplateParseError(
              line = HeaderLineNumber,
              column = Some(expectedHeader),
              code = "header_mismatch",
              message = templateFileErrorMessage
            )
        }

        extraColumnError.toSeq ++ headerErrors
    }
}
