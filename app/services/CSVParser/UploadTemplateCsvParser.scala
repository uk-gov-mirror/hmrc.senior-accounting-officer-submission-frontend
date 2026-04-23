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

import com.univocity.parsers.csv.{CsvParser, CsvParserSettings}
import models.upload.TemplateParseError
import models.upload.TemplateParseResult
import models.upload.TemplateParseResult.Invalid
import play.api.i18n.{Messages, MessagesApi}
import services.CSVParser.UploadTemplateCsvSchema.*

import scala.jdk.CollectionConverters.*
import scala.util.Try

import java.io.StringReader
import javax.inject.Inject

class UploadTemplateCsvParser @Inject() (
    messagesApi: MessagesApi,
    structureValidator: UploadTemplateStructureValidator,
    rowParser: UploadTemplateRowParser
) {

  private def message(key: String, messages: Messages): String = messages(key)

  private def templateFileErrorMessage(messages: Messages): String =
    message(TemplateFileErrorMessageKey, messages)

  private def rowErrorMessages(messages: Messages) = UploadTemplateRowErrorMessages(
    companyName = message(CompanyNameErrorMessageKey, messages),
    companyUtr = message(CompanyUtrErrorMessageKey, messages),
    companyCrn = message(CompanyCrnErrorMessageKey, messages),
    companyType = message(CompanyTypeErrorMessageKey, messages),
    companyStatus = message(CompanyStatusErrorMessageKey, messages),
    financialYearEndDate = message(FinancialYearEndDateErrorMessageKey, messages),
    taxRegime = message(TaxRegimeErrorMessageKey, messages),
    certificateType = message(CertificateTypeErrorMessageKey, messages),
    additionalInformation = message(AdditionalInformationErrorMessageKey, messages)
  )

  def parse(csv: String, messages: Messages = messagesApi.preferred(Seq.empty)): TemplateParseResult = {
    Try {
      val rows   = parseCsvRows(csv)
      val errors =
        structureValidator.validateSectionRow(rows.lift(SectionRowIndex), templateFileErrorMessage(messages)) ++
          structureValidator.validateHeaderRow(rows.lift(HeaderRowIndex), templateFileErrorMessage(messages))

      errors match {
        case Nil      => rowParser.parseDataRows(rows, rowErrorMessages(messages), templateFileErrorMessage(messages))
        case nonEmpty => Invalid(nonEmpty)
      }
    }.fold(
      err =>
        Invalid(
          Seq(
            TemplateParseError(
              0,
              None,
              "invalid_csv",
              s"Unable to parse CSV content: ${err.getMessage}"
            )
          )
        ),
      identity
    )
  }

  private def parseCsvRows(csv: String): Vector[CsvRow] = {
    val settings = CsvParserSettings()
    settings.setLineSeparatorDetectionEnabled(true)
    settings.setReadInputOnSeparateThread(false)
    settings.setNullValue("")
    settings.setEmptyValue("")
    settings.setMaxColumns(64)
    settings.setMaxCharsPerColumn(100000)

    val parser    = CsvParser(settings)
    val sanitized = csv.stripPrefix("\uFEFF")

    parser.parseAll(StringReader(sanitized)).asScala.iterator.map(_.toVector).toVector
  }
}

object UploadTemplateCsvParser {
  val ExpectedHeaders: Seq[String] = UploadTemplateCsvSchema.ExpectedHeaders
}
