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

package models.upload

import base.SpecBase
import play.api.libs.json.{JsSuccess, Json}
import services.csvparser.UploadTemplateCsvSchema.{Column, TemplateError}

import java.time.LocalDate

class UploadTemplateTableDataSpec extends SpecBase {

  "UploadTemplateTableData json format" - {

    "must round-trip parsed rows and errors" in {
      val tableData = UploadTemplateTableData(
        rows = Seq(
          ParsedSubmissionRow(
            notification = NotificationFields(
              companyName = "Acme Plc",
              companyUtr = CompanyUtr("0123456789"),
              companyCrn = Some(CompanyCrn("12345678")),
              companyType = CompanyType.PLC,
              companyStatus = CompanyStatus.Active,
              financialYearEndDate = LocalDate.of(2025, 12, 31)
            ),
            certificate = Some(
              CertificateFields(
                corporationTax = true,
                valueAddedTax = false,
                paye = false,
                insurancePremiumTax = false,
                stampDutyLandTax = false,
                stampDutyReserveTax = false,
                petroleumRevenueTax = false,
                customsDuties = false,
                exciseDuties = false,
                bankLevy = false,
                certificateType = Some(CertificateType.Qualified),
                qualificationStatement = Some("Reason")
              )
            )
          )
        ),
        errors = Seq(
          TemplateParseError(
            line = 9,
            column = Some(Column.Utr),
            error = TemplateError.UtrError
          )
        )
      )

      Json.toJson(tableData).validate[UploadTemplateTableData] mustBe JsSuccess(tableData)
    }
  }
}
