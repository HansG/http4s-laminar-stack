package example.frontend

import com.raquo.laminar.api.L.*
import org.scalajs.dom

object Client:

  case class SearchBox private (node: Element, signal: Signal[String])

  object SearchBox:
    def create =
      val node = input(
        `type` := "text",
        idAttr := "search-filter"
      )

      val stream =
        node.events(onInput).mapTo(node.ref.value).startWith("")

      new SearchBox(node, stream)

  case class PrefixOnlyCheckbox private (node: Element, signal: Signal[Boolean])

  object PrefixOnlyCheckbox:
    def create =
      val node = input(
        `type` := "checkbox",
        idAttr := "prefix-only-filter"
      )

      val stream =
        node
          .events(onChange)
          .mapTo(node.ref.checked)
          .startWith(node.ref.checked)

      new PrefixOnlyCheckbox(node, stream)
  end PrefixOnlyCheckbox

  def SearchApp(api: Api, debounce: Int = 250) =
    val searchBox  = SearchBox.create
    val prefixOnly = PrefixOnlyCheckbox.create

    val debounced =
      if debounce > 0 then
        searchBox.signal
          .combineWith(prefixOnly.signal)
          .composeChanges(_.debounce(debounce))
      else searchBox.signal.combineWith(prefixOnly.signal)

    val resolved =
      debounced
        .flatMap(r => Signal.fromFuture(api.post(r._1, r._2)))
        .map {
          case None => img(src := "/assets/ajax-loader.gif")
          case Some(Right(response)) =>
            ul(
              response.suggestions.map(sug => li(sug))
            )
          case Some(Left(err)) => b(err.toString)
        }

    val results =
      div(idAttr := "results", child <-- resolved)

    div(
      div("Search: ", searchBox.node),
      div("Prefix only", prefixOnly.node),
      results
    )
  end SearchApp


  object HelloWorld:

    val nameVar = Var(initial = "world")

    val rootElement = div(
      label("Your name: "),
      input(
        onMountFocus,
        placeholder := "Enter your name here",
        onInput.mapToValue --> nameVar
      ),
      span(
        "Hello, ",
        child.text <-- nameVar.signal.map(_.toUpperCase)
      )
    )

    // In most other examples, containerNode will be set to this behind the scenes
//    val containerNode = dom.document.querySelector("#mdoc-html-run0")
//    render(containerNode, rootElement)

  object Counter:
    def make(label: String, initialStep: Int): HtmlElement = {

      val allowedSteps = List(1, 2, 3, 5, 10)

      val stepVar = Var(initialStep)

      val diffBus = new EventBus[Int]

      val countSignal: Signal[Int] = diffBus.events.foldLeft(initial = 0)(_ + _)

      div(
        p(
          "Step: ",
          select(
            value <-- stepVar.signal.map(_.toString),
            onChange.mapToValue.map(_.toInt) --> stepVar,
            allowedSteps.map { step =>
              option(value := step.toString, step)
            }
          )
        ),
        p(
          label + ": ",
          b(child.text <-- countSignal),
          " ",
          // Two different ways to get stepVar's value:
          button(
            "–",
            onClick.mapTo(-1 * stepVar.now()) --> diffBus
          ),
          button(
            "+",
            onClick.mapTo(stepVar.now()) --> diffBus
//            onClick.compose(_.sample(stepVar.signal)) --> diffBus
          )
        )
      )
    }

    val app = div(
      h1("Let's count!"),
      make("Sheep", initialStep = 3)
    )



  def main(args: Array[String]): Unit =
      documentEvents.onDomContentLoaded.foreach { _ =>
        render(dom.document.getElementById("appContainer"), div(SearchApp(FutureApi), HelloWorld.rootElement)  )
      }(unsafeWindowOwner)



end Client
