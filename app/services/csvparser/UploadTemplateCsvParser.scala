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

import com.github.tototoshi.csv.CSVReader
import models.upload.TemplateParseResult.Invalid
import models.upload.{TemplateParseError, TemplateParseResult}
import services.csvparser.UploadTemplateCsvSchema.*

import scala.util.{Failure, Success, Try}

import java.io.StringReader
import javax.inject.Inject

class UploadTemplateCsvParser @Inject() (
    structureValidator: UploadTemplateStructureValidator,
    rowParser: UploadTemplateRowParser
) {

  def parse(
      csv: String,
      notificationOnly: Boolean
  ): TemplateParseResult = {
    Try(parseCsvRows(csv)) match {
      case Failure(err) =>
        Invalid(
          Seq(
            TemplateParseError(
              line = 0,
              column = None,
              error = TemplateError.InvalidTemplateError
            )
          )
        )

      case Success(rows) =>
        val errors =
          structureValidator.validateSectionRow(rows.lift(SectionRowIndex)) ++
            structureValidator.validateHeaderRow(rows.lift(HeaderRowIndex))

        errors match {
          case Nil =>
            rowParser.parseDataRows(
              rows,
              notificationOnly
            )
          case nonEmpty => Invalid(nonEmpty)
        }
    }
  }

  private def parseCsvRows(csv: String): Vector[CsvRow] = {
    val sanitized = csv.stripPrefix("\uFEFF")
    val reader    = CSVReader.open(StringReader(sanitized))

    try reader.all().iterator.map(_.toVector).toVector
    finally reader.close()
  }
}

object UploadTemplateCsvParser {
  val ExpectedHeaders: Seq[String] = UploadTemplateCsvSchema.ExpectedHeaders
}
