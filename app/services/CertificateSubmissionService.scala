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

package services

import connectors.CertificateSubmissionConnector
import models.UserAnswers
import models.certificate.*
import models.upload.{CertificateFields, NotificationFields, ParsedSubmissionRow}
import pages.certificate.*
import play.api.Logging
import play.api.libs.json.Json
import repositories.SessionRepository
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

import java.time.format.DateTimeFormatter
import javax.inject.Inject

class CertificateSubmissionService @Inject() (
    connector: CertificateSubmissionConnector,
    sessionRepository: SessionRepository
)(using ExecutionContext)
    extends Logging {

  import CertificateSubmissionService.*

  def submit(
      userId: String,
      userAnswers: UserAnswers,
      token: String
  )(using HeaderCarrier): Future[CertificateSubmissionResult] =
    buildRequest(userAnswers) match {
      case Left(error) =>
        logger.warn(s"Certificate submission could not be built: $error")
        Future.successful(CertificateSubmissionResult.MissingData)
      case Right(request) =>
        sessionRepository.claimCertificateSubmissionToken(userId, token).flatMap {
          case false =>
            logger.warn("Certificate submission token was missing or already used")
            Future.successful(CertificateSubmissionResult.Duplicate)
          case true =>
            connector
              .submit(request)
              .flatMap { response =>
                sessionRepository
                  .set(userAnswers.copy(data = Json.obj()))
                  .recover { case e =>
                    logger.warn(s"Certificate ${response.certificateRef} submitted but wiping journey data failed", e)
                    true
                  }
                  .map(_ => CertificateSubmissionResult.Submitted(response.certificateRef))
              }
              .recover { case e =>
                logger.error("Certificate submission failed", e)
                CertificateSubmissionResult.Failed
              }
        }
    }

  private def buildRequest(
      userAnswers: UserAnswers
  ): Either[String, CertificateSubmissionRequest] =
    for {
      saoName   <- userAnswers.get(CertificateSaoFullNamePage).toRight("missing SAO name")
      saoEmail  <- userAnswers.get(CertificateSaoEmailPage).toRight("missing SAO email")
      tableData <- userAnswers.get(CertificateUploadTemplateTablePage).toRight("missing uploaded certificate data")
      companies = tableData.rows.collect { case ParsedSubmissionRow(notification, Some(certificate)) =>
        toCompany(notification, certificate)
      }
      _ <- Either.cond(companies.nonEmpty, (), "missing companies")
    } yield CertificateSubmissionRequest(
      submitterName = submitterName(userAnswers),
      saoName = saoName,
      saoEmail = saoEmail,
      companies = companies,
      remarks = userAnswers.getNullable(CertificateAdditionalInformationPage)
    )

  private def submitterName(userAnswers: UserAnswers): Option[String] =
    userAnswers
      .get(CertificateWhoIsSubmittingPage)
      .collect { case CertificateWhoIsSubmitting.StandIn =>
        userAnswers.get(CertificateDeclarationStandInPage).map(_.StandInName)
      }
      .flatten

  private def toCompany(
      notification: NotificationFields,
      certificate: CertificateFields
  ): CertificateSubmissionCompany =
    CertificateSubmissionCompany(
      crn = notification.companyCrn.map(_.value),
      utr = notification.companyUtr.value,
      name = notification.companyName,
      accPeriodEnd = notification.financialYearEndDate.format(DateTimeFormatter.ISO_LOCAL_DATE),
      status = notification.companyStatus,
      `type` = notification.companyType,
      isCorporationTaxQualified = certificate.corporationTax,
      isVatQualified = certificate.valueAddedTax,
      isPayeQualified = certificate.paye,
      isInsurancePremiumTaxQualified = certificate.insurancePremiumTax,
      isStampDutyLandTaxQualified = certificate.stampDutyLandTax,
      isStampDutyReserveTaxQualified = certificate.stampDutyReserveTax,
      isPetroleumRevenueTaxQualified = certificate.petroleumRevenueTax,
      isCustomsDutiesQualified = certificate.customsDuties,
      isExciseDutiesQualified = certificate.exciseDuties,
      isBankLevyQualified = certificate.bankLevy,
      qualificationStatement = certificate.qualificationStatement
    )

}

object CertificateSubmissionService {
  enum CertificateSubmissionResult {
    case Submitted(certificateRef: String)
    case MissingData
    case Duplicate
    case Failed
  }
}
