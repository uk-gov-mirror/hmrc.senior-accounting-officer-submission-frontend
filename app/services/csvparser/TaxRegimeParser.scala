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
      row: IndexedSeq[String]
  ): TaxRegimeParseResult = {
    val (corporationTax, corporationTaxErrors) =
      parseTaxRegimeValue(lineNumber, Column.CorporationTax, row(Column.CorporationTax.columnIndex))
    val (valueAddedTax, valueAddedTaxErrors) =
      parseTaxRegimeValue(lineNumber, Column.Vat, row(Column.Vat.columnIndex))
    val (paye, payeErrors) =
      parseTaxRegimeValue(lineNumber, Column.Paye, row(Column.Paye.columnIndex))
    val (insurancePremiumTax, insurancePremiumTaxErrors) =
      parseTaxRegimeValue(
        lineNumber,
        Column.InsurancePremiumTax,
        row(Column.InsurancePremiumTax.columnIndex)
      )
    val (stampDutyLandTax, stampDutyLandTaxErrors) =
      parseTaxRegimeValue(
        lineNumber,
        Column.StampDutyLandTax,
        row(Column.StampDutyLandTax.columnIndex)
      )
    val (stampDutyReserveTax, stampDutyReserveTaxErrors) =
      parseTaxRegimeValue(
        lineNumber,
        Column.StampDutyReserveTax,
        row(Column.StampDutyReserveTax.columnIndex)
      )
    val (petroleumRevenueTax, petroleumRevenueTaxErrors) =
      parseTaxRegimeValue(
        lineNumber,
        Column.PetroleumRevenueTax,
        row(Column.PetroleumRevenueTax.columnIndex)
      )
    val (customsDuties, customsDutiesErrors) =
      parseTaxRegimeValue(lineNumber, Column.CustomsDuties, row(Column.CustomsDuties.columnIndex))
    val (exciseDuties, exciseDutiesErrors) =
      parseTaxRegimeValue(lineNumber, Column.ExciseDuties, row(Column.ExciseDuties.columnIndex))
    val (bankLevy, bankLevyErrors) =
      parseTaxRegimeValue(lineNumber, Column.BankLevy, row(Column.BankLevy.columnIndex))

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
      column: Column,
      value: String
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
              column = Some(column),
              error = TemplateError.TaxRegimeError
            )
          )
        )
    }
}
