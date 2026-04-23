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

import models.upload.{CertificateType, TemplateParseError}
import services.CSVParser.UploadTemplateCsvSchema.*

import javax.inject.Inject

final case class CertificateParseResult(
    certificateType: Option[CertificateType],
    additionalInformation: Option[String],
    errors: Vector[TemplateParseError]
)

class CertificateRulesValidator @Inject() () {

  def parse(
      lineNumber: Int,
      certificateTypeValue: String,
      additionalInformationValue: String,
      taxFlags: ParsedTaxFlags,
      rowErrorMessages: UploadTemplateRowErrorMessages
  ): CertificateParseResult = {
    val (certificateType, certificateErrors) =
      parseCertificateTypeValue(lineNumber, certificateTypeValue, taxFlags, rowErrorMessages)

    val additionalInformationErrors =
      validateAdditionalInformationValue(
        lineNumber,
        additionalInformationValue,
        taxFlags.hasAnySelected,
        rowErrorMessages
      )

    val additionalInformation =
      Option(additionalInformationValue).filter(_.nonEmpty)

    CertificateParseResult(
      certificateType = certificateType,
      additionalInformation = additionalInformation,
      errors = certificateErrors ++ additionalInformationErrors
    )
  }

  private def parseCertificateTypeValue(
      lineNumber: Int,
      value: String,
      taxFlags: ParsedTaxFlags,
      rowErrorMessages: UploadTemplateRowErrorMessages
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
            column = Some(ExpectedHeaders(CertificateTypeIndex)),
            code = "invalid_certificate_type",
            message = rowErrorMessages.certificateType
          )
        )
      )
    } else {
      (certificateType, Vector.empty)
    }
  }

  private def validateAdditionalInformationValue(
      lineNumber: Int,
      value: String,
      hasAnyTaxRegimeSelected: Boolean,
      rowErrorMessages: UploadTemplateRowErrorMessages
  ): Vector[TemplateParseError] =
    if hasAnyTaxRegimeSelected && value.isEmpty then {
      Vector(
        TemplateParseError(
          line = lineNumber,
          column = Some(ExpectedHeaders(AdditionalInformationIndex)),
          code = "missing_qualified_reason",
          message = rowErrorMessages.additionalInformation
        )
      )
    } else {
      Vector.empty
    }
}
