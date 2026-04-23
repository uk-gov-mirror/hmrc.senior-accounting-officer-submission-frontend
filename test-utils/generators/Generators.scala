/*
 * Copyright 2025 HM Revenue & Customs
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

package generators

import org.scalacheck.Arbitrary.*
import org.scalacheck.Gen.*
import org.scalacheck.{Gen, Shrink}

import java.time.*

trait Generators extends ModelGenerators {

  given dontShrink: Shrink[String] = Shrink.shrinkAny

  val maxEmailLength           = 254
  val specialChars: List[Char] = List('<', '>', '&')

  def genIntersperseString(gen: Gen[String], value: String, frequencyV: Int = 1, frequencyN: Int = 10): Gen[String] = {

    val genValue: Gen[Option[String]] = Gen.frequency(frequencyN -> None, frequencyV -> Gen.const(Some(value)))

    for {
      seq1 <- gen
      seq2 <- Gen.listOfN(seq1.length, genValue)
    } yield {
      seq1.toSeq.zip(seq2).foldLeft("") {
        case (acc, (n, Some(v))) =>
          acc + n + v
        case (acc, (n, _)) =>
          acc + n
      }
    }
  }

  def intsInRangeWithCommas(min: Int, max: Int): Gen[String] = {
    val numberGen = choose[Int](min, max).map(_.toString)
    genIntersperseString(numberGen, ",")
  }

  def intsLargerThanMaxValue: Gen[BigInt] =
    arbitrary[BigInt] suchThat (x => x > Int.MaxValue)

  def intsSmallerThanMinValue: Gen[BigInt] =
    arbitrary[BigInt] suchThat (x => x < Int.MinValue)

  def nonNumerics: Gen[String] =
    alphaStr suchThat (_.size > 0)

  def decimals: Gen[String] =
    arbitrary[BigDecimal]
      .suchThat(_.abs < Int.MaxValue)
      .suchThat(!_.isValidInt)
      .map("%f".format(_))

  def intsBelowValue(value: Int): Gen[Int] =
    arbitrary[Int] suchThat (_ < value)

  def intsAboveValue(value: Int): Gen[Int] =
    arbitrary[Int] suchThat (_ > value)

  def intsOutsideRange(min: Int, max: Int): Gen[Int] =
    arbitrary[Int] suchThat (x => x < min || x > max)

  def nonBooleans: Gen[String] =
    arbitrary[String]
      .suchThat(_.nonEmpty)
      .suchThat(_ != "true")
      .suchThat(_ != "false")

  def nonEmptyString: Gen[String] =
    arbitrary[String] suchThat (_.nonEmpty)

  def stringsWithMaxLength(maxLength: Int): Gen[String] =
    for {
      length <- choose(1, maxLength)
      chars  <- listOfN(length, arbitrary[Char])
    } yield chars.mkString

  def stringsLongerThan(minLength: Int): Gen[String] = for {
    maxLength <- (minLength * 2).max(100)
    length    <- Gen.chooseNum(minLength + 1, maxLength)
    chars     <- listOfN(length, arbitrary[Char].withFilter(char => !specialChars.contains(char)))
  } yield chars.mkString

  def stringsExceptSpecificValues(excluded: Seq[String]): Gen[String] =
    nonEmptyString suchThat (!excluded.contains(_))

  def oneOf[T](xs: Seq[Gen[T]]): Gen[T] =
    if xs.isEmpty then {
      throw new IllegalArgumentException("oneOf called on empty collection")
    } else {
      val vector = xs.toVector
      choose(0, vector.size - 1).flatMap(vector(_))
    }

  def datesBetween(min: LocalDate, max: LocalDate): Gen[LocalDate] = {

    def toMillis(date: LocalDate): Long =
      date.atStartOfDay.atZone(ZoneOffset.UTC).toInstant.toEpochMilli

    Gen.choose(toMillis(min), toMillis(max)).map { millis =>
      Instant.ofEpochMilli(millis).atOffset(ZoneOffset.UTC).toLocalDate
    }
  }

  def genLocalTime: Gen[LocalTime] =
    Gen
      .choose(
        min = LocalTime.MIN.toSecondOfDay.toLong,
        max = LocalTime.MAX.toSecondOfDay.toLong
      )
      .map(LocalTime.ofSecondOfDay)

  def genZonedDateTime: Gen[ZonedDateTime] =
    for {
      date <- datesBetween(min = LocalDate.of(0, 1, 1), max = LocalDate.of(9999, 12, 31))
      time <- genLocalTime
    } yield ZonedDateTime.of(date, time, ZoneOffset.UTC)

  def genLongEmailAddresses: Gen[String] = {
    val safeChar = Gen.oneOf(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9'))
    for {
      excessLength <- Gen.choose(1, 20)
      localPartLength = maxEmailLength + excessLength - 11
      localPart <- Gen.listOfN(localPartLength, safeChar).map(_.mkString)
    } yield s"$localPart@domain.com"
  }

  def genValidEmailAddress: Gen[String] = {
    val safeChar = Gen.oneOf(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9'))
    for {
      localLength <- Gen.choose(1, 20)
      local       <- Gen.listOfN(localLength, safeChar).map(_.mkString)
      domain      <- Gen.listOfN(5, safeChar).map(_.mkString)
      tld         <- Gen.oneOf("com", "org", "net", "uk", "io")
    } yield s"$local@$domain.$tld"
  }

  def genInvalidEmailAddresses: Gen[String] = {
    Gen.oneOf(
      Gen.const("notAnEmail"),
      Gen.const("missing@domain"),
      Gen.const("missing@domain."),
      Gen.const("@noDomain.com"),
      Gen.const("missingAtSign.com")
    )
  }
}

object Generators extends Generators
