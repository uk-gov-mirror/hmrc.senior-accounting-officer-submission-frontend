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

package services.csvparser

import models.upload.TemplateParseError
import services.csvparser.UploadTemplateCsvSchema.*

import javax.inject.Inject

class UploadTemplateStructureValidator @Inject() () {

  def validateSectionRow(rowOpt: Option[CsvRow]): Seq[TemplateParseError] =
    rowOpt match {
      case None =>
        Seq(
          TemplateParseError(
            line = SectionLineNumber,
            column = None,
            error = TemplateError.InvalidTemplateError
          )
        )
      case Some(row) =>
        val notificationSection = cellValue(row, NotificationSectionIndex)
        val certificateSection  = cellValue(row, CertificateSectionIndex)

        Seq(
          Option.when(notificationSection != NotificationSectionExpected)(
            TemplateParseError(
              line = SectionLineNumber,
              column = None,
              error = TemplateError.InvalidTemplateError
            )
          ),
          Option.when(certificateSection != CertificateSectionExpected)(
            TemplateParseError(
              line = SectionLineNumber,
              column = None,
              error = TemplateError.InvalidTemplateError
            )
          )
        ).flatten
    }

  private def normaliseLineEnding(string: String): String =
    string.replaceAll("\r\n", "\n").replaceAll("\r", "\n")

  def validateHeaderRow(rowOpt: Option[CsvRow]): Seq[TemplateParseError] =
    rowOpt match {
      case None =>
        Seq(
          TemplateParseError(
            line = HeaderLineNumber,
            column = None,
            error = TemplateError.InvalidTemplateError
          )
        )
      case Some(row) =>
        val extraColumnError = Option.when(row.drop(ExpectedHeaders.length).exists(_.trim.nonEmpty))(
          TemplateParseError(
            line = HeaderLineNumber,
            column = None,
            error = TemplateError.InvalidTemplateError
          )
        )

        val headerErrors = ExpectedHeaders.zipWithIndex.collect {
          case (expectedHeader, idx) if normaliseLineEnding(cellValue(row, idx)) != expectedHeader =>

            TemplateParseError(
              line = HeaderLineNumber,
              column = None,
              error = TemplateError.InvalidTemplateError
            )
        }

        extraColumnError.toSeq ++ headerErrors
    }
}
