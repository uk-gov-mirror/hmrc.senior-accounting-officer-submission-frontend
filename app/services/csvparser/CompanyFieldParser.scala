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

import models.upload.*
import services.csvparser.UploadTemplateCsvSchema.*

import scala.util.Try

import java.time.LocalDate
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, ResolverStyle}
import java.time.temporal.ChronoField
import javax.inject.Inject

import CompanyFieldParser.*

final case class ParsedCompanyFields(
    companyName: String,
    companyUtr: CompanyUtr,
    companyCrn: Option[CompanyCrn],
    companyType: CompanyType,
    companyStatus: CompanyStatus,
    financialYearEndDate: LocalDate
)

final case class CompanyFieldParseResult(
    fields: Option[ParsedCompanyFields],
    errors: Vector[TemplateParseError]
)

class CompanyFieldParser @Inject() () {

  def parse(
      lineNumber: Int,
      row: IndexedSeq[String]
  ): CompanyFieldParseResult = {
    val (companyName, companyNameErrors) =
      parseCompanyNameValue(lineNumber, row(Column.CompanyName.columnIndex))
    val (companyUtr, companyUtrErrors) =
      parseCompanyUtrValue(lineNumber, row(Column.Utr.columnIndex))
    val (companyCrn, companyCrnErrors) =
      parseCompanyCrnValue(lineNumber, row(Column.Crn.columnIndex))
    val (companyType, companyTypeErrors) =
      parseCompanyTypeValue(lineNumber, row(Column.CompanyType.columnIndex))
    val (companyStatus, companyStatusErrors) =
      parseCompanyStatusValue(lineNumber, row(Column.CompanyStatus.columnIndex))
    val (financialYearEndDate, financialYearEndDateErrors) =
      parseFinancialYearEndDateValue(lineNumber, row(Column.FinancialYearEndDate.columnIndex))

    val errors =
      companyNameErrors ++ companyUtrErrors ++ companyCrnErrors ++ companyTypeErrors ++
        companyStatusErrors ++ financialYearEndDateErrors

    val fields =
      for {
        name    <- companyName
        utr     <- companyUtr
        cType   <- companyType
        cStatus <- companyStatus
        fye     <- financialYearEndDate
      } yield ParsedCompanyFields(
        companyName = name,
        companyUtr = utr,
        companyCrn = companyCrn,
        companyType = cType,
        companyStatus = cStatus,
        financialYearEndDate = fye
      )

    CompanyFieldParseResult(fields = fields, errors = errors)
  }

  private def parseCompanyNameValue(
      lineNumber: Int,
      value: String
  ): (Option[String], Vector[TemplateParseError]) =
    Option(value.trim).filter(value => value.nonEmpty && value.length <= companyNameMaxLength) match {
      case Some(validName) =>
        (Some(validName), Vector.empty)
      case None =>
        (
          None,
          Vector(
            TemplateParseError(
              line = lineNumber,
              column = Some(Column.CompanyName),
              error = TemplateError.CompanyNameError
            )
          )
        )
    }

  private def parseCompanyUtrValue(
      lineNumber: Int,
      value: String
  ): (Option[CompanyUtr], Vector[TemplateParseError]) =
    CompanyUtr
      .fromString(value)
      .map(parsed => (Some(parsed), Vector.empty))
      .getOrElse(
        (
          None,
          Vector(
            TemplateParseError(
              line = lineNumber,
              column = Some(Column.Utr),
              error = TemplateError.UtrError
            )
          )
        )
      )

  private def parseCompanyCrnValue(
      lineNumber: Int,
      value: String
  ): (Option[CompanyCrn], Vector[TemplateParseError]) =
    if value.isEmpty then (None, Vector.empty)
    else
      CompanyCrn
        .fromString(value)
        .map(parsed => (Some(parsed), Vector.empty))
        .getOrElse(
          (
            None,
            Vector(
              TemplateParseError(
                line = lineNumber,
                column = Some(Column.Crn),
                error = TemplateError.CrnError
              )
            )
          )
        )

  private def parseCompanyTypeValue(
      lineNumber: Int,
      value: String
  ): (Option[CompanyType], Vector[TemplateParseError]) =
    CompanyType
      .fromString(value)
      .filter(ct => ct == CompanyType.PLC || ct == CompanyType.LTD)
      .map(parsed => (Some(parsed), Vector.empty))
      .getOrElse(
        (
          None,
          Vector(
            TemplateParseError(
              line = lineNumber,
              column = Some(Column.CompanyType),
              error = TemplateError.CompanyTypeError
            )
          )
        )
      )

  private def parseCompanyStatusValue(
      lineNumber: Int,
      value: String
  ): (Option[CompanyStatus], Vector[TemplateParseError]) =
    CompanyStatus
      .fromString(value)
      .map(parsed => (Some(parsed), Vector.empty))
      .getOrElse(
        (
          None,
          Vector(
            TemplateParseError(
              line = lineNumber,
              column = Some(Column.CompanyStatus),
              error = TemplateError.CompanyStatusError
            )
          )
        )
      )

  private def parseFinancialYearEndDateValue(
      lineNumber: Int,
      value: String
  ): (Option[LocalDate], Vector[TemplateParseError]) =
    Try(LocalDate.parse(value, FinancialYearEndDateFormatter)).toOption
      .map(parsed => (Some(parsed), Vector.empty))
      .getOrElse(
        (
          None,
          Vector(
            TemplateParseError(
              line = lineNumber,
              column = Some(Column.FinancialYearEndDate),
              error = TemplateError.FinancialYearEndDateError
            )
          )
        )
      )
}

object CompanyFieldParser {
  val companyNameMaxLength: Int = 160

  val FinancialYearEndDateFormatter: DateTimeFormatter =
    DateTimeFormatterBuilder()
      .appendPattern("dd/MM/yyyy")
      .parseDefaulting(ChronoField.ERA, 1)
      .toFormatter
      .withResolverStyle(ResolverStyle.STRICT)

}
