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

import play.api.libs.json.*

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import java.time.format.ResolverStyle

final case class ParsedSubmissionRow(
    notification: NotificationFields,
    certificate: CertificateFields
)

object ParsedSubmissionRow {
  given OFormat[ParsedSubmissionRow] = Json.format[ParsedSubmissionRow]
}

final case class NotificationFields(
    companyName: String,
    companyUtr: CompanyUtr,
    companyCrn: Option[CompanyCrn],
    companyType: CompanyType,
    companyStatus: CompanyStatus,
    financialYearEndDate: LocalDate
) {
  def financialYearEndDateDisplay: String =
    financialYearEndDate.format(NotificationFields.JsonDateFormatter)
}

object NotificationFields {
  private[upload] val JsonDateFormatter =
    DateTimeFormatter.ofPattern("dd/MM/yyyy").withResolverStyle(ResolverStyle.SMART)

  given Format[LocalDate] = Format(
    Reads {
      case JsString(value) =>
        try JsSuccess(LocalDate.parse(value, JsonDateFormatter))
        catch {
          case _: DateTimeParseException => JsError(s"Invalid date value: $value")
        }
      case _ =>
        JsError("Expected JSON string")
    },
    Writes(localDate => JsString(localDate.format(JsonDateFormatter)))
  )

  given OFormat[NotificationFields] = Json.format[NotificationFields]
}

final case class CompanyUtr(value: String)

object CompanyUtr {
  private val CompanyUtrRegex = "^[0-9]{10}$".r

  def fromString(value: String): Option[CompanyUtr] =
    Option.when(CompanyUtrRegex.matches(value))(CompanyUtr(value))

  given Format[CompanyUtr] = Format(
    Reads {
      case JsString(value) =>
        fromString(value).map(JsSuccess(_)).getOrElse(JsError(s"Invalid company UTR: $value"))
      case _ =>
        JsError("Expected JSON string")
    },
    Writes(companyUtr => JsString(companyUtr.value))
  )
}

final case class CompanyCrn(value: String)

object CompanyCrn {
  private val CompanyCrnRegex = "^[A-Za-z0-9]{1,8}$".r

  def fromString(value: String): Option[CompanyCrn] =
    Option.when(CompanyCrnRegex.matches(value))(CompanyCrn(value))

  given Format[CompanyCrn] = Format(
    Reads {
      case JsString(value) =>
        fromString(value).map(JsSuccess(_)).getOrElse(JsError(s"Invalid company CRN: $value"))
      case _ =>
        JsError("Expected JSON string")
    },
    Writes(companyCrn => JsString(companyCrn.value))
  )
}

final case class CertificateFields(
    corporationTax: Boolean,
    valueAddedTax: Boolean,
    paye: Boolean,
    insurancePremiumTax: Boolean,
    stampDutyLandTax: Boolean,
    stampDutyReserveTax: Boolean,
    petroleumRevenueTax: Boolean,
    customsDuties: Boolean,
    exciseDuties: Boolean,
    bankLevy: Boolean,
    certificateType: Option[CertificateType],
    additionalInformation: Option[String]
)

object CertificateFields {
  given OFormat[CertificateFields] = Json.format[CertificateFields]
}

enum CompanyType {
  case LTD
  case PLC
}

object CompanyType {
  def fromString(value: String): Option[CompanyType] =
    value match {
      case "LTD" => Some(CompanyType.LTD)
      case "PLC" => Some(CompanyType.PLC)
      case _     => None
    }

  given Format[CompanyType] = enumFormat(fromString)
}

enum CompanyStatus {
  case Active
  case Dormant
  case Administration
  case Liquidation
}

object CompanyStatus {
  def fromString(value: String): Option[CompanyStatus] =
    value match {
      case "Active"         => Some(CompanyStatus.Active)
      case "Dormant"        => Some(CompanyStatus.Dormant)
      case "Administration" => Some(CompanyStatus.Administration)
      case "Liquidation"    => Some(CompanyStatus.Liquidation)
      case _                => None
    }

  given Format[CompanyStatus] = enumFormat(fromString)
}

enum CertificateType {
  case Qualified
  case Unqualified
}

object CertificateType {
  def fromString(value: String): Option[CertificateType] =
    value match {
      case "Qualified"   => Some(CertificateType.Qualified)
      case "Unqualified" => Some(CertificateType.Unqualified)
      case _             => None
    }

  given Format[CertificateType] = enumFormat(fromString)
}

private def enumFormat[T](lookup: String => Option[T]): Format[T] =
  Format(
    Reads[T] {
      case JsString(value) =>
        lookup(value) match {
          case Some(parsed) => JsSuccess(parsed)
          case None         => JsError(s"Unknown enum value: $value")
        }
      case _ =>
        JsError("Expected JSON string")
    },
    Writes[T](value => JsString(value.toString))
  )
