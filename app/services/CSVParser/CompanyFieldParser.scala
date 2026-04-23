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

import models.upload.*
import services.CSVParser.UploadTemplateCsvSchema.*

import java.time.LocalDate
import java.time.format.DateTimeParseException
import javax.inject.Inject

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
      row: IndexedSeq[String],
      rowErrorMessages: UploadTemplateRowErrorMessages
  ): CompanyFieldParseResult = {
    val (companyName, companyNameErrors) =
      parseCompanyNameValue(lineNumber, row(CompanyNameIndex), rowErrorMessages)
    val (companyUtr, companyUtrErrors) =
      parseCompanyUtrValue(lineNumber, row(CompanyUtrIndex), rowErrorMessages)
    val (companyCrn, companyCrnErrors) =
      parseCompanyCrnValue(lineNumber, row(CompanyCrnIndex), rowErrorMessages)
    val (companyType, companyTypeErrors) =
      parseCompanyTypeValue(lineNumber, row(CompanyTypeIndex), rowErrorMessages)
    val (companyStatus, companyStatusErrors) =
      parseCompanyStatusValue(lineNumber, row(CompanyStatusIndex), rowErrorMessages)
    val (financialYearEndDate, financialYearEndDateErrors) =
      parseFinancialYearEndDateValue(lineNumber, row(FinancialYearEndDateIndex), rowErrorMessages)

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
      value: String,
      rowErrorMessages: UploadTemplateRowErrorMessages
  ): (Option[String], Vector[TemplateParseError]) =
    Option(value).filter(_.matches(CompanyNameRegex)).filter(_.length <= 105) match {
      case Some(validName) =>
        (Some(validName), Vector.empty)
      case None =>
        (
          None,
          Vector(
            TemplateParseError(
              line = lineNumber,
              column = Some(ExpectedHeaders(CompanyNameIndex)),
              code = "invalid_company_name",
              message = rowErrorMessages.companyName
            )
          )
        )
    }

  private def parseCompanyUtrValue(
      lineNumber: Int,
      value: String,
      rowErrorMessages: UploadTemplateRowErrorMessages
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
              column = Some(ExpectedHeaders(CompanyUtrIndex)),
              code = "invalid_company_utr",
              message = rowErrorMessages.companyUtr
            )
          )
        )
      )

  private def parseCompanyCrnValue(
      lineNumber: Int,
      value: String,
      rowErrorMessages: UploadTemplateRowErrorMessages
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
                column = Some(ExpectedHeaders(CompanyCrnIndex)),
                code = "invalid_company_crn",
                message = rowErrorMessages.companyCrn
              )
            )
          )
        )

  private def parseCompanyTypeValue(
      lineNumber: Int,
      value: String,
      rowErrorMessages: UploadTemplateRowErrorMessages
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
              column = Some(ExpectedHeaders(CompanyTypeIndex)),
              code = "invalid_company_type",
              message = rowErrorMessages.companyType
            )
          )
        )
      )

  private def parseCompanyStatusValue(
      lineNumber: Int,
      value: String,
      rowErrorMessages: UploadTemplateRowErrorMessages
  ): (Option[CompanyStatus], Vector[TemplateParseError]) =
    CompanyStatus
      .fromString(value)
      .filter(_ => value.matches(CompanyStatusRegex))
      .map(parsed => (Some(parsed), Vector.empty))
      .getOrElse(
        (
          None,
          Vector(
            TemplateParseError(
              line = lineNumber,
              column = Some(ExpectedHeaders(CompanyStatusIndex)),
              code = "invalid_company_status",
              message = rowErrorMessages.companyStatus
            )
          )
        )
      )

  private def parseFinancialYearEndDateValue(
      lineNumber: Int,
      value: String,
      rowErrorMessages: UploadTemplateRowErrorMessages
  ): (Option[LocalDate], Vector[TemplateParseError]) =
    try (Some(LocalDate.parse(value, FinancialYearEndDateFormatter)), Vector.empty)
    catch {
      case _: DateTimeParseException =>
        (
          None,
          Vector(
            TemplateParseError(
              line = lineNumber,
              column = Some(ExpectedHeaders(FinancialYearEndDateIndex)),
              code = "invalid_financial_year_end_date",
              message = rowErrorMessages.financialYearEndDate
            )
          )
        )
    }
}
