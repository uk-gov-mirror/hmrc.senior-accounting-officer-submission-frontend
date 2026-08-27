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

package base

import base.ViewSpecBase.*
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements
import org.scalactic.source.Position
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.{Messages, MessagesApi}
import play.api.mvc.{Call, Request}
import play.api.test.FakeRequest
import play.twirl.api.{BaseScalaTemplate, Format, HtmlFormat}

import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import scala.util.Try

class ViewSpecBase[T <: BaseScalaTemplate[HtmlFormat.Appendable, Format[HtmlFormat.Appendable]]: ClassTag]
    extends SpecBase
    with GuiceOneAppPerSuite {
  def SUT: T = app.injector.instanceOf[T]

  given request: Request[?] = FakeRequest()
  given Messages            = app.injector.instanceOf[MessagesApi].preferred(Seq.empty)

  extension (doc: Document) {
    def getMainContent: Element = doc.getElementById("main-content")

    def getConfirmationPanel: Element =
      util
        .Try(doc.select(".govuk-panel.govuk-panel--confirmation").get(0))
        .getOrElse(throw RuntimeException("No Confirmation Panel found"))

    def createTestsWithStandardPageElements(
        pageTitle: String,
        pageHeading: String,
        showBackLink: Boolean,
        showIsThisPageNotWorkingProperlyLink: true,
        hasError: Boolean
    )(using pos: Position): Unit = {
      createTestWithPageTitle(pageTitle = pageTitle, hasError = hasError)
      createTestWithPageHeading(pageHeading = pageHeading)
      createTestWithBackLink(show = showBackLink)
      createTestWithIsThisPageNotWorkingProperlyLink
    }

    def createTestsForInputWidth(): Unit = {
      "must have input with expected class 'govuk-input--width-20'" in {
        doc.getMainContent.select("input.govuk-input--width-20").size() mustBe 1
      }
    }

    def createTestWithPageTitle(pageTitle: String, hasError: Boolean)(using pos: Position): Unit =
      "must generate a view with the correct title" in {
        val errorPrefix = if hasError then "Error: " else ""
        doc.title mustBe s"$errorPrefix$pageTitle - $expectedServiceName - GOV.UK"
      }

    def createTestWithPageHeading(pageHeading: String)(using
        pos: Position
    ): Unit =
      "must generate a view with the correct page heading" in {
        val actualH1 = doc.getMainContent.getElementsByTag("h1")
        withClue(s"the page must contain only a single <h1> with content '$pageHeading'\n") {
          actualH1.get(0).text() mustBe pageHeading
          actualH1.size() mustBe 1
        }
      }

    def createTestForInsetText(text: String): Unit = {
      val insetTextElement = doc.getMainContent.select(".govuk-inset-text")
      "must have one inset string" in {
        insetTextElement.size() mustBe 1
      }

      s"must have expected inset string of $text" in {
        insetTextElement.text() mustBe text
      }
    }

    def createTestWithBackLink(show: Boolean)(using pos: Position): Unit =
      if show then
        "must show a backlink " in {
          val backLink = doc.getElementsByClass("govuk-back-link")
          withClue(
            "backlink is not found\n"
          ) {
            backLink.size() mustBe 1
          }
        }
      else
        "must not show a backlink" in {
          val elements = doc.getElementsByClass("govuk-back-link")
          withClue(
            "a backlink was found\n"
          ) {
            elements.size() mustBe 0
          }
        }

    def createTestsWithSubmissionButtons(
        action: Call,
        buttonTexts: Seq[String]
    )(using
        pos: Position
    ): Unit = {
      def target = doc.getMainContent
      s"must have a form which submits to '${action.method} ${action.url}'" in {
        val form = target.select("form")
        form.attr("method") mustBe action.method
        form.attr("action") mustBe action.url
        form.size() mustBe 1
      }
      s"must have ${buttonTexts.size} number of buttons" in {
        val buttons = target.select("button")

        buttons.size() mustBe buttonTexts.size
      }
      buttonTexts.zipWithIndex
        .foreach((buttonText, i) => {
          s"must have a submit button with text '$buttonText'" in {
            val button = target.select("button").get(i)
            withClue(
              s"Submit Button with text '$buttonText' not found\n"
            ) {
              button.text() mustBe buttonText
            }
          }
        })
    }

    def createTestWithIsThisPageNotWorkingProperlyLink(using
        pos: Position
    ): Unit =
      "must generate a view with 'Is this page not working properly? (opens in new tab)' " in {
        val helpLink = doc.getMainContent.select("a.govuk-link.hmrc-report-technical-issue")
        withClue(
          "help link not found, both contact-frontend.host and contact-frontend.serviceId must be set in the configs\n"
        ) {
          helpLink.text() mustBe "Is this page not working properly? (opens in new tab)"
          helpLink.size() mustBe 1
        }

        java.net.URI(helpLink.get(0).attributes.get("href")).getQuery must include(s"service=$expectedServiceId")
      }

    def createTestsWithOrWithoutError(hasError: Boolean): Unit =
      if hasError then
        "must show an error" in {
          val elements = doc.getElementsByClass("govuk-error-summary")
          withClue("error message must be shown\n") {
            elements.size mustBe 1
          }

          val errorSummary      = elements.first
          val errorSummaryTitle = errorSummary.getElementsByClass("govuk-error-summary__title")

          errorSummaryTitle.text mustBe "There is a problem"
        }
      else
        "must not show an error" in {
          val elements = doc.getElementsByClass("govuk-error-summary govuk-form-group--error")
          withClue("error message must not be shown\n") {
            elements.isEmpty() mustBe true
          }
        }
  }

  extension (target: => Document | Element) {
    private def resolve: Element = target match {
      case doc: Document => doc.getMainContent
      case _             => target
    }

    private def safeSelect(cssQuery: String): Elements =
      if cssQuery.nonEmpty
      then target.resolve.select(cssQuery)
      else Elements()

    def getParagraphs(includeHelpLink: Boolean = false): Iterable[Element] =
      target.resolve.select(if includeHelpLink then "p" else excludeHelpLinkAndErrorMessageParagraphsSelector).asScala

    def getPanelTitle: Element =
      util
        .Try(target.resolve.select("h1.govuk-panel__title").get(0))
        .getOrElse(throw RuntimeException("No panel title found"))

    def getPanelBody: Element =
      util
        .Try(target.resolve.select("div.govuk-panel__body").get(0))
        .getOrElse(throw RuntimeException("No panel body found"))

    def createTestMustShowNumberOfInputs(expectedCount: Int)(using pos: Position): Unit = {
      s"must have a $expectedCount of input(s)" in {
        val selector =
          """input[type="text"], input[type="password"], input[type="email"], input[type="search"], input[type="tel"], input[type="url"], input[type="number"]"""

        def elements = target.resolve.select(selector).asScala

        withClue(s"Expected a $expectedCount of inputs but found ${elements.size}\n") {
          elements.size mustBe expectedCount
        }
      }
    }

    def createTestMustShowNumberOfTextareas(expectedCount: Int)(using pos: Position): Unit = {
      s"must have a $expectedCount of text area(s)" in {
        val selector = "textarea"

        def elements = target.resolve.select(selector).asScala

        withClue(s"Expected a $expectedCount of text area(s) but found ${elements.size}\n") {
          elements.size mustBe expectedCount
        }
      }
    }

    def createTestMustShowTextarea(
        name: String,
        label: String,
        value: String,
        hint: Option[String],
        hasError: Boolean
    )(using pos: Position): Unit = {

      s"for textarea '$name'" - {

        def textareaElements = target.resolve.select(s"textarea[name=$name].govuk-textarea")

        s"textarea with name '$name' must exist on the page" in {
          withClue(s"textarea with the name '$name' not found\n'") {
            textareaElements.size mustBe 1
          }
        }

        def textAreaElement = textareaElements.get(0)

        s"textarea with name '$name' must have a label '$label' with correct id and text" in {
          val textareaId = textAreaElement.attr("id")
          withClue(s"textarea with an 'id' attribute not found\n") {
            textareaId must not be ""
          }

          val labels = target.resolve.select(s"""label[for="$textareaId"]""")
          withClue(s"label for '$textareaId' not found\n") {
            labels.size() must be > 0
          }
          withClue(s"label text does not match expected label text '$label'\n") {
            labels.get(0).text mustEqual label
          }

          val erroredFormGroup = target.resolve.select(s""".govuk-error-summary a[href="#$textareaId"]""")
          if hasError then {
            withClue("error content must be shown\n") {
              erroredFormGroup.size mustBe 1
            }
          } else {
            withClue("error content must not be shown\n") {
              erroredFormGroup.size mustBe 0
            }
          }
        }

        s"textarea with name '$name' must have value of '$value'" in {
          withClue(s"textarea with name '$name' does not have a value attribute '$value'\n") {
            textAreaElement.text mustEqual value
          }
        }

        hint match {
          case Some(expectedHintText) => {
            def hintSelector = textAreaElement
              .attr("aria-describedby")
              .split(" ")
              .filter(_.nonEmpty)
              .map("#" + _ + ".govuk-hint")
              .mkString(",")

            def hints = target.resolve.safeSelect(hintSelector)

            s"textarea with name '$name' must have a hint with values '$expectedHintText'" in {
              withClue(s"for textarea with name '$name' hint element not found\n") {
                hints.size() must be > 0
              }
              withClue(s"textarea with name '$name' multiple hint elements found\n") {
                hints.size() mustBe 1
              }
              withClue(
                s"textarea with name '$name' hint text does not match expected value '$expectedHintText' not found\n"
              ) {
                hints.get(0).text mustEqual expectedHintText
              }
            }
          }
          case None =>
            s"textarea with name '$name' must not have an associated hint" in {
              val hints = target.resolve.select(".govuk-hint")

              withClue(s"for textarea with name '$name' hint element not found\n") {
                hints.size() mustBe 0
              }
            }
        }

        if hasError then {
          s"textarea with name '$name' must show an associated error message when field has error" in {

            val errorMessageElements = target.resolve.safeSelect(s".govuk-error-summary a[href=\"#$name\"]")

            withClue(s"textarea does not have expected error message with id '$name'\n") {
              errorMessageElements.size mustBe 1
            }
          }
        } else {
          s"textarea with name '$name' must not show an associated error message when field has no error" in {
            val errorMessageElements = target.resolve.select(".govuk-error-message")

            withClue(s"textarea has unexpected error message with id '$name'\n") {
              errorMessageElements.size mustBe 0
            }
          }
        }
      }
    }

    def createTestMustShowTextInput(
        name: String,
        label: String,
        value: String,
        hint: Option[String],
        hasError: Boolean
    )(using pos: Position): Unit = {

      s"for input '$name'" - {

        def inputElements = target.resolve.select(s"input[name=$name].govuk-input")

        s"input with name '$name' must exist on the page" in {
          withClue(s"input with the name '$name' not found\n'") {
            inputElements.size mustBe 1
          }
        }

        def inputElement = inputElements.get(0)

        s"input with name '$name' must have a label '$label' with correct id and text" in {
          val inputId = inputElement.attr("id")
          withClue(s"input with an 'id' attribute not found\n") {
            inputId must not be ""
          }

          val labels = target.resolve.select(s"""label[for="$inputId"]""")
          withClue(s"label for '$inputId' not found\n") {
            labels.size() must be > 0
          }
          withClue(s"label text does not match expected label text '$label'\n") {
            labels.get(0).text mustEqual label
          }

          val erroredFormGroup = target.resolve.select(s""".govuk-form-group--error label[for="$inputId"]""")
          if hasError then {
            withClue("error content must be shown\n") {
              erroredFormGroup.size mustBe 1
            }
          } else {
            withClue("error content must not be shown\n") {
              erroredFormGroup.size mustBe 0
            }
          }
        }

        s"input with name '$name' must have value of '$value'" in {
          withClue(s"input with name '$name' does not have a value attribute '$value'\n") {
            inputElement.attr("value") mustEqual value
          }
        }

        hint match {
          case Some(expectedHintText) => {
            def hintSelector = inputElement
              .attr("aria-describedby")
              .split(" ")
              .filter(_.nonEmpty)
              .map("#" + _ + ".govuk-hint")
              .mkString(",")

            def hints = target.resolve.safeSelect(hintSelector)

            s"input with name '$name' must have a hint with values '$expectedHintText'" in {
              withClue(s"for input with name '$name' hint element not found\n") {
                hints.size() must be > 0
              }
              withClue(s"input with name '$name' multiple hint elements found\n") {
                hints.size() mustBe 1
              }
              withClue(
                s"input with name '$name' hint text does not match expected value '$expectedHintText' not found\n"
              ) {
                hints.get(0).text mustEqual expectedHintText
              }
            }
          }
          case None =>
            s"input with name '$name' must not have an associated hint" in {
              val hints = target.resolve.select(".govuk-hint")

              withClue(s"for input with name '$name' hint element not found\n") {
                hints.size() mustBe 0
              }
            }
        }

        if hasError then {
          s"input with name '$name' must show an associated error message when field has error" in {
            val errorMessageSelector = inputElement
              .attr("aria-describedby")
              .split(" ")
              .filter(_.nonEmpty)
              .map("#" + _ + ".govuk-error-message")
              .mkString(",")

            val errorMessageElements = target.resolve.safeSelect(errorMessageSelector)

            withClue(s"input does not have expected error message with id '$name'\n") {
              errorMessageElements.size mustBe 1
            }
          }
        } else {
          s"input with name '$name' must not show an associated error message when field has no error" in {
            val errorMessageElements = target.resolve.select(".govuk-error-message")

            withClue(s"input has unexpected error message with id '$name'\n") {
              errorMessageElements.size mustBe 0
            }
          }
        }
      }
    }

    def createTestsWithASingleTextInput(
        name: String,
        label: String,
        value: String,
        hint: Option[String],
        hasError: Boolean
    )(using pos: Position): Unit = {
      createTestMustShowNumberOfInputs(1)
      createTestMustShowTextInput(
        name = name,
        label = label,
        value = value,
        hint = hint,
        hasError = hasError
      )
    }

    def createTestMustShowHint(expectedHint: String)(using pos: Position): Unit = {
      s"must have a hint with values '$expectedHint'" in {
        val hintElement = target.resolve.getElementsByClass("govuk-hint").asScala.headOption
        hintElement match {
          case Some(hint) => hint.text mustEqual expectedHint
          case None       => fail(s"no hint element found\n")
        }
      }
    }

    def createTestsWithRadioButtons(
        name: String,
        radios: Seq[RadioButton],
        isChecked: Option[RadioButton],
        hasError: Boolean
    )(using pos: Position): Unit = {
      def matchingRadioSelector = s"input[type=radio][name=$name]"
      def labelCssSelector      = {
        val matchingRadioButtons = target.resolve.select(matchingRadioSelector).asScala
        if matchingRadioButtons.isEmpty then {
          // this method can't result in an empty string, and we know this would yield with no result
          s"$matchingRadioSelector+label"
        } else
          matchingRadioButtons
            .map(element => s"label[for=${element.attr("id")}]")
            .mkString(",")
      }
      val hintCssSelector = "div.govuk-hint.govuk-radios__hint"

      def fieldsetElement = target.resolve.select("fieldset.govuk-fieldset")

      def errorMessageSelector = fieldsetElement
        .attr("aria-describedby")
        .split(" ")
        .filter(_.nonEmpty)
        .map("#" + _ + ".govuk-error-message")
        .mkString(",")

      def errorMessageElements = target.resolve.safeSelect(errorMessageSelector)

      if hasError then {
        "must show error message" in {
          withClue("no error message found\n") {
            errorMessageElements.size mustBe 1
          }
        }
      } else {
        "must not show error message" in {
          withClue("error message found\n") {
            errorMessageElements.size mustBe 0
          }
        }
      }

      val hints = radios.map(_.hint).collect { case Some(hint) =>
        hint
      }

      createTestWithCountOfElement(
        selector = hintCssSelector,
        count = hints.size,
        description = "radio button hint"
      )
      createTestsWithOrderOfElements(
        selector = hintCssSelector,
        texts = hints,
        description = "radio button hint"
      )

      createTestWithCountOfElement(
        selector = labelCssSelector,
        count = radios.size,
        description = "radio button label"
      )
      createTestsWithOrderOfElements(
        selector = labelCssSelector,
        texts = radios.map(_.label),
        description = "radio button label"
      )

      createTestWithCountOfElement(
        selector = matchingRadioSelector,
        count = radios.size,
        description = s"radio buttons for $name"
      )

      target.resolve
        .select(matchingRadioSelector)
        .asScala
        .zip(radios)
        .foreach { case (element, radio) =>
          def expectedText = radio.value
          def elementValue = element.attr("value")
          s"radio button value $elementValue must match $expectedText" in {
            elementValue mustEqual expectedText
          }

          def shouldBeChecked: Boolean = isChecked.contains(radio)

          def elementIsChecked: Boolean = element.hasAttr("checked")

          if shouldBeChecked then {
            s"radio button value $elementValue must be checked" in {
              withClue(s"The '$elementValue' radio button was not checked!\n") {
                elementIsChecked mustEqual true
              }
            }
          } else {
            s"radio button value $elementValue must not be checked" in {
              withClue(s"The '$elementValue' radio button was erroneously checked!\n") {
                elementIsChecked mustEqual false
              }
            }
          }

        }
    }

    def createTestsWithDateInput(values: DateFieldValues, hasError: Boolean): Unit = {
      def fieldsetElement = target.resolve.select("fieldset.govuk-fieldset")

      "must contain a single fieldset" in {
        fieldsetElement.size mustBe 1
      }

      "must display the correct input fields" in {
        fieldsetElement.select("""input#value\.day""").size() mustBe 1
        fieldsetElement.select("""input#value\.month""").size() mustBe 1
        fieldsetElement.select("""input#value\.year""").size() mustBe 1
      }

      "must display the correct label" in {
        fieldsetElement.select("label[for=value.day]").text() mustBe "Day"
        fieldsetElement.select("label[for=value.month]").text() mustBe "Month"
        fieldsetElement.select("label[for=value.year]").text() mustBe "Year"
      }

      "must show correct values" in {
        fieldsetElement.select("""input#value\.day""").attr("value") mustBe values.day
        fieldsetElement.select("""input#value\.month""").attr("value") mustBe values.month
        fieldsetElement.select("""input#value\.year""").attr("value") mustBe values.year
      }

      def errorMessageSelector = fieldsetElement
        .attr("aria-describedby")
        .split(" ")
        .filter(_.nonEmpty)
        .map("#" + _ + ".govuk-error-message")
        .mkString(",")

      def errorMessageElements = target.resolve.safeSelect(errorMessageSelector)

      if hasError then {
        "must show error message" in {
          withClue("no error message found\n") {
            errorMessageElements.size mustBe 1
          }
        }
      } else {
        "must not show error message" in {
          withClue("error message found\n") {
            errorMessageElements.size mustBe 0
          }
        }
      }
    }

    def createTestsWithSubmissionButton(
        action: Call,
        buttonText: String
    )(using
        pos: Position
    ): Unit = {
      s"must have a form which submits to '${action.method} ${action.url}'" in {
        val form = target.resolve.select("form")
        form.attr("method") mustBe action.method
        form.attr("action") mustBe action.url
        form.size() mustBe 1
      }

      "must have a submit button" - {
        s"must have text '$buttonText'" in {
          val button = target.resolve.select("button[type=submit]")
          withClue(
            s"Submit Button with text '$buttonText' not found\n"
          ) {
            button.text() mustBe buttonText
            button.size() mustBe 1
          }
        }

        s"""must have an id of "submit"""" in {
          val button = target.resolve.select("button[type=submit]")
          withClue(s"Submit Button not found\n") {
            button.size() mustBe 1
          }
          withClue(s"""Submit Button's ID is not "submit"\n""") {
            button.attr("id") mustBe "submit"
          }
        }
      }
    }

    def createTestWithoutElements(byClass: String)(using pos: Position): Unit =
      s"must not show the element of class $byClass" in {
        val elements = target.resolve.getElementsByClass(byClass)
        elements.size() mustBe 0
      }

    def createTestWithText(text: String)(using pos: Position): Unit =
      s"must have text '$text'" in {
        target.resolve.text() mustBe text
      }

    private def createTestWithCountOfElement(
        selector: String,
        count: Int,
        description: String
    )(using pos: Position): Unit =
      s"must have $count of $description" in {
        val elements = target.resolve.select(selector).asScala
        withClue(s"Expected $count $description but found ${elements.size}\n") {
          elements.size mustBe count
        }
      }

    private def createTestsWithOrderOfElements(
        selector: String,
        texts: Seq[String],
        description: String
    )(using pos: Position): Unit =
      texts.zipWithIndex.foreach { case (expectedText, index) =>
        s"must have a $description with content '$expectedText' (check ${index + 1})" in {
          val elements = target.resolve.select(selector).asScala

          withClue(s"$description with content '$expectedText' not found\n") {
            val element =
              Try(elements(index))
                .getOrElse(fail(s"Index $index out of bounds for length ${elements.size}"))
            element.text() mustEqual expectedText
          }
        }
      }

    def createTestsWithParagraphs(
        paragraphs: Seq[String]
    )(using
        pos: Position
    ): Unit = {
      createTestWithCountOfElement(
        selector = excludeHelpLinkAndErrorMessageParagraphsSelector,
        count = paragraphs.size,
        description = "paragraphs"
      )
      createTestsWithOrderOfElements(
        selector = excludeHelpLinkAndErrorMessageParagraphsSelector,
        texts = paragraphs,
        description = "paragraphs"
      )

      "all paragraphs must have the expected CSS class" in {
        def paragraphs =
          target.resolve.select(excludeHelpLinkAndErrorMessageParagraphsSelector).asScala

        paragraphs.foreach(paragraph =>
          withClue(s"$paragraph did not have the expected CSS class\n") {
            paragraph.className() must include("govuk-body")
          }
        )
      }
    }

    def createTestsWithBulletPoints(
        bullets: Seq[String]
    )(using
        pos: Position
    ): Unit = {
      createTestWithCountOfElement(
        selector = "ul.govuk-list--bullet li",
        count = bullets.size,
        description = "bullets"
      )
      createTestsWithOrderOfElements(
        selector = "ul.govuk-list--bullet li",
        texts = bullets,
        description = "bullets"
      )
    }

    def createTestsWithNumberedItems(
        numberedItems: Seq[String]
    )(using
        pos: Position
    ): Unit = {
      createTestWithCountOfElement(
        selector = "ol.govuk-list--number li",
        count = numberedItems.size,
        description = "numbered items"
      )
      createTestsWithOrderOfElements(
        selector = "ol.govuk-list--number li",
        texts = numberedItems,
        description = "numbered items"
      )
    }

    def createTestsWithCaption(
        caption: String
    )(using pos: Position): Unit = {
      createTestWithCountOfElement(
        selector = "span.govuk-caption-m",
        count = 1,
        description = "captions"
      )
      createTestsWithOrderOfElements(
        selector = "span.govuk-caption-m",
        texts = Seq(caption),
        description = "captions"
      )
    }

    def createTestsWithLargeCaption(
        caption: String
    )(using pos: Position): Unit = {
      createTestWithCountOfElement(
        selector = "span.govuk-caption-l",
        count = 1,
        description = "captions"
      )
      createTestsWithOrderOfElements(
        selector = "span.govuk-caption-l",
        texts = Seq(caption),
        description = "captions"
      )
    }

    def createTestWithLink(
        linkText: String,
        destinationUrl: String
    )(using
        pos: Position
    ): Unit =
      s"must have expected link with correct text: $linkText and correct url $destinationUrl within provided element" in {
        val element       = target.resolve
        val link: Element = if element.tagName() == "a" then {
          element
        } else {
          val links = element.select("a").asScala
          withClue(s"Expected to find exactly one link in the element but found ${links.size}\n") {
            links.size mustBe 1
          }
          links.head
        }
        withClue(s"link text was not as expected. Got ${link.text()}, expected '$linkText'\n") {
          link.text mustBe linkText
        }
        withClue(s"link href was not as expected. Got ${link.attr("href")}, expected '$destinationUrl'\n") {
          link.attr("href") mustBe destinationUrl
        }

        withClue(s"link must have expected CSS class\n") {
          link.className() must include("govuk-link")
        }
      }

  }

  @inline def radio(value: String, label: String, hint: Option[String] = None): RadioButton =
    RadioButton(value = value, label = label, hint = hint)

}

object ViewSpecBase {
  val expectedServiceName = "Senior Accounting Officer notification and certificate"
  val expectedServiceId   = "senior-accounting-officer-submission-frontend"

  val excludeHelpLinkAndErrorMessageParagraphsSelector =
    "p:not(:has(a.hmrc-report-technical-issue), .govuk-error-message)"

  final case class RadioButton(value: String, label: String, hint: Option[String])
  final case class DateFieldValues(day: String, month: String, year: String)
}
