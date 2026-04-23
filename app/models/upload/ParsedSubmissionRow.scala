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
import java.time.format.DateTimeFormatterBuilder
import java.time.format.DateTimeParseException
import java.time.format.ResolverStyle
import java.time.temporal.ChronoField

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
    DateTimeFormatterBuilder()
      .appendPattern("dd/MM/yyyy")
      .parseDefaulting(ChronoField.ERA, 1)
      .toFormatter
      .withResolverStyle(ResolverStyle.STRICT)

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
    Option.when(CompanyUtrRegex.matches(value.trim))(CompanyUtr(value.trim))

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
  private val CompanyCrnRegex = "^[A-Za-z0-9]{8}$".r

  def fromString(value: String): Option[CompanyCrn] =
    Option.when(CompanyCrnRegex.matches(value.trim))(CompanyCrn(value.trim))

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
    value.trim match {
      case normalized if normalized.equalsIgnoreCase("LTD") => Some(CompanyType.LTD)
      case normalized if normalized.equalsIgnoreCase("PLC") => Some(CompanyType.PLC)
      case _                                                => None
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
    value.trim match {
      case normalized if normalized.equalsIgnoreCase("Active")         => Some(CompanyStatus.Active)
      case normalized if normalized.equalsIgnoreCase("Dormant")        => Some(CompanyStatus.Dormant)
      case normalized if normalized.equalsIgnoreCase("Administration") => Some(CompanyStatus.Administration)
      case normalized if normalized.equalsIgnoreCase("Liquidation")    => Some(CompanyStatus.Liquidation)
      case _                                                           => None
    }

  given Format[CompanyStatus] = enumFormat(fromString)
}

enum CertificateType {
  case Qualified
  case Unqualified
}

object CertificateType {
  def serialized(value: CertificateType): String =
    value match {
      case CertificateType.Qualified   => "qualified"
      case CertificateType.Unqualified => "unqualified"
    }

  def fromString(value: String): Option[CertificateType] =
    value.trim match {
      case normalized if normalized.equalsIgnoreCase("Qualified")   => Some(CertificateType.Qualified)
      case normalized if normalized.equalsIgnoreCase("Unqualified") => Some(CertificateType.Unqualified)
      case _                                                        => None
    }

  given Format[CertificateType] = Format(
    Reads[CertificateType] {
      case JsString(value) =>
        fromString(value) match {
          case Some(parsed) => JsSuccess(parsed)
          case None         => JsError(s"Unknown enum value: $value")
        }
      case _ =>
        JsError("Expected JSON string")
    },
    Writes[CertificateType](value => JsString(serialized(value)))
  )
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
