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

import models.upload.{CertificateType, TemplateParseError}
import services.csvparser.UploadTemplateCsvSchema.*

import javax.inject.Inject

import CertificateRulesValidator.*

final case class CertificateParseResult(
    certificateType: Option[CertificateType],
    qualificationStatement: Option[String],
    errors: Vector[TemplateParseError]
)

class CertificateRulesValidator @Inject() () {

  def parse(
      lineNumber: Int,
      certificateTypeValue: String,
      qualificationStatementValue: String,
      taxFlags: ParsedTaxFlags
  ): CertificateParseResult = {
    val (certificateType, certificateErrors) =
      parseCertificateTypeValue(lineNumber, certificateTypeValue, taxFlags)

    val qualificationStatementErrors =
      validateQualificationStatementValue(
        lineNumber,
        qualificationStatementValue,
        taxFlags.hasAnySelected
      )

    val qualificationStatement =
      Option(qualificationStatementValue).filter(_.nonEmpty)

    CertificateParseResult(
      certificateType = certificateType,
      qualificationStatement = qualificationStatement,
      errors = certificateErrors ++ qualificationStatementErrors
    )
  }

  private def parseCertificateTypeValue(
      lineNumber: Int,
      value: String,
      taxFlags: ParsedTaxFlags
  ): (Option[CertificateType], Vector[TemplateParseError]) = {
    val parsedFromValue =
      if value.isEmpty then None
      else CertificateType.fromString(value)

    val certificateType =
      parsedFromValue.orElse(Option.when(value.isEmpty && taxFlags.hasAnySelected)(CertificateType.Qualified))

    val isInvalidMissingOrUnknown =
      (value.nonEmpty && parsedFromValue.isEmpty) || (value.isEmpty && !taxFlags.hasAnySelected)

    val hasCrossFieldMismatch =
      certificateType.contains(CertificateType.Unqualified) && taxFlags.hasAnySelected ||
        certificateType.contains(CertificateType.Qualified) && !taxFlags.hasAnySelected

    if isInvalidMissingOrUnknown || hasCrossFieldMismatch then {
      (
        None,
        Vector(
          TemplateParseError(
            line = lineNumber,
            column = Some(Column.CertificateType),
            error = TemplateError.CertificateTypeError
          )
        )
      )
    } else {
      (certificateType, Vector.empty)
    }
  }

  private def validateQualificationStatementValue(
      lineNumber: Int,
      value: String,
      hasAnyTaxRegimeSelected: Boolean
  ): Vector[TemplateParseError] =
    if hasAnyTaxRegimeSelected && value.isEmpty then {
      Vector(
        TemplateParseError(
          line = lineNumber,
          column = Some(Column.QualificationStatement),
          error = TemplateError.QualificationStatementMissingError
        )
      )
    } else if hasAnyTaxRegimeSelected && value.length > qualificationStatementMaxLength then {
      Vector(
        TemplateParseError(
          line = lineNumber,
          column = Some(Column.QualificationStatement),
          error = TemplateError.QualificationStatementTooLongError
        )
      )
    } else if !hasAnyTaxRegimeSelected && value.nonEmpty then {
      Vector(
        TemplateParseError(
          line = lineNumber,
          column = Some(Column.QualificationStatement),
          error = TemplateError.QualificationStatementProhibitedError
        )
      )
    } else {
      Vector.empty
    }
}

object CertificateRulesValidator {
  val qualificationStatementMaxLength: Int = 5000
}
