import play.core.PlayVersion
import play.sbt.PlayImport.*
import sbt.Keys.libraryDependencies
import sbt.*

object AppDependencies {

  private val bootstrapVersion = "9.19.0"
  private val hmrcMongoVersion = "2.7.0"
  private val hmrcPlayFrontend = "12.8.0"
  private val poiVersion       = "5.5.1"
  private val univocityVersion = "2.9.1"

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"            %% "bootstrap-frontend-play-30" % bootstrapVersion,
    "uk.gov.hmrc"            %% "play-frontend-hmrc-play-30" % hmrcPlayFrontend,
    "uk.gov.hmrc.mongo"      %% "hmrc-mongo-play-30"         % hmrcMongoVersion,
    "org.apache.poi"          % "poi"                        % poiVersion,
    "org.apache.poi"          % "poi-ooxml"                  % poiVersion,
    "com.univocity"           % "univocity-parsers"          % univocityVersion,
    "io.github.openhtmltopdf" % "openhtmltopdf-pdfbox"       % "1.1.37"
  )

  val test: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"       %% "bootstrap-test-play-30"  % bootstrapVersion,
    "uk.gov.hmrc.mongo" %% "hmrc-mongo-test-play-30" % hmrcMongoVersion,
    "org.scalatestplus" %% "scalacheck-1-18"         % "3.2.19.0"
  ).map(_ % Test)

  def apply(): Seq[ModuleID] = compile ++ test
}
