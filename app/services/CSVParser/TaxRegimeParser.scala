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

final case class ParsedTaxFlags(
    corporationTax: Boolean,
    valueAddedTax: Boolean,
    paye: Boolean,
    insurancePremiumTax: Boolean,
    stampDutyLandTax: Boolean,
    stampDutyReserveTax: Boolean,
    petroleumRevenueTax: Boolean,
    customsDuties: Boolean,
    exciseDuties: Boolean,
    bankLevy: Boolean
) {
  val hasAnySelected: Boolean =
    corporationTax || valueAddedTax || paye || insurancePremiumTax || stampDutyLandTax ||
      stampDutyReserveTax || petroleumRevenueTax || customsDuties || exciseDuties || bankLevy
}

final case class TaxRegimeParseResult(
    flags: ParsedTaxFlags,
    errors: Vector[TemplateParseError]
)

class TaxRegimeParser @Inject() {

  def parse(
      lineNumber: Int,
      row: IndexedSeq[String],
      rowErrorMessages: UploadTemplateRowErrorMessages
  ): TaxRegimeParseResult = {
    val (corporationTax, corporationTaxErrors) =
      parseTaxRegimeValue(lineNumber, CorporationTaxIndex, row(CorporationTaxIndex), rowErrorMessages)
    val (valueAddedTax, valueAddedTaxErrors) =
      parseTaxRegimeValue(lineNumber, ValueAddedTaxIndex, row(ValueAddedTaxIndex), rowErrorMessages)
    val (paye, payeErrors) =
      parseTaxRegimeValue(lineNumber, PayeIndex, row(PayeIndex), rowErrorMessages)
    val (insurancePremiumTax, insurancePremiumTaxErrors) =
      parseTaxRegimeValue(lineNumber, InsurancePremiumTaxIndex, row(InsurancePremiumTaxIndex), rowErrorMessages)
    val (stampDutyLandTax, stampDutyLandTaxErrors) =
      parseTaxRegimeValue(lineNumber, StampDutyLandTaxIndex, row(StampDutyLandTaxIndex), rowErrorMessages)
    val (stampDutyReserveTax, stampDutyReserveTaxErrors) =
      parseTaxRegimeValue(lineNumber, StampDutyReserveTaxIndex, row(StampDutyReserveTaxIndex), rowErrorMessages)
    val (petroleumRevenueTax, petroleumRevenueTaxErrors) =
      parseTaxRegimeValue(lineNumber, PetroleumRevenueTaxIndex, row(PetroleumRevenueTaxIndex), rowErrorMessages)
    val (customsDuties, customsDutiesErrors) =
      parseTaxRegimeValue(lineNumber, CustomsDutiesIndex, row(CustomsDutiesIndex), rowErrorMessages)
    val (exciseDuties, exciseDutiesErrors) =
      parseTaxRegimeValue(lineNumber, ExciseDutiesIndex, row(ExciseDutiesIndex), rowErrorMessages)
    val (bankLevy, bankLevyErrors) =
      parseTaxRegimeValue(lineNumber, BankLevyIndex, row(BankLevyIndex), rowErrorMessages)

    val errors =
      List(
        corporationTaxErrors,
        valueAddedTaxErrors,
        payeErrors,
        insurancePremiumTaxErrors,
        stampDutyLandTaxErrors,
        stampDutyReserveTaxErrors,
        petroleumRevenueTaxErrors,
        customsDutiesErrors,
        exciseDutiesErrors,
        bankLevyErrors
      ).iterator.flatten.toVector

    TaxRegimeParseResult(
      flags = ParsedTaxFlags(
        corporationTax = corporationTax,
        valueAddedTax = valueAddedTax,
        paye = paye,
        insurancePremiumTax = insurancePremiumTax,
        stampDutyLandTax = stampDutyLandTax,
        stampDutyReserveTax = stampDutyReserveTax,
        petroleumRevenueTax = petroleumRevenueTax,
        customsDuties = customsDuties,
        exciseDuties = exciseDuties,
        bankLevy = bankLevy
      ),
      errors = errors
    )
  }

  private def parseTaxRegimeValue(
      lineNumber: Int,
      columnIndex: Int,
      value: String,
      rowErrorMessages: UploadTemplateRowErrorMessages
  ): (Boolean, Vector[TemplateParseError]) =
    value.toLowerCase match {
      case ""  => (false, Vector.empty)
      case "x" => (true, Vector.empty)
      case _   =>
        (
          false,
          Vector(
            TemplateParseError(
              line = lineNumber,
              column = Some(ExpectedHeaders(columnIndex)),
              code = "invalid_tax_regime_value",
              message = rowErrorMessages.taxRegime
            )
          )
        )
    }
}
