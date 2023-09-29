package example.frontend

import com.raquo.laminar.api.L.*
import com.raquo.laminar.api.L.given
import example.frontend.tries.Client1
import org.scalajs.dom

object Client:

  object SearcBoxApp:
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

    def app(api: Api, debounce: Int = 250) =
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
    end app
  end SearcBoxApp



  object Time:

    val tickStream = EventStream.periodic(1000)

    val timeApp = div(
      div(
        "Tick #: ",
        child.text <-- tickStream.map(_.toString)
      ),
      div(
        "Random #: ",
        child.text <-- tickStream.mapTo(scala.util.Random.nextInt() % 100)
      )
    )

    val clickBus = new EventBus[Unit]

    val maybeAlertStream = EventStream.merge(
      clickBus.events.mapTo(Some(span("Just clicked!"))),
      clickBus.events.flatMap { _ =>
        EventStream.fromValue(None, emitOnce = true).delay(500)
      }
    )

    val clickApp = div(
      button(onClick.mapTo(()) --> clickBus, "Click me"),
      child.maybe <-- maybeAlertStream
    )


    def emailError(email: String): Option[String] =
      if (email.isEmpty)
        Some("Please fill out email")
      else if (!email.contains('@'))
        Some("Invalid email!")
      else
        None

    val inputBus = new EventBus[String]

    val debouncedErrorStream: EventStream[Option[String]] =
      inputBus.events
        .debounce(1000)
        .map(emailError)

    val app = div(
      span(
        label("Your email: "),
        input(
          value <-- inputBus.events,
          onInput.mapToValue --> inputBus
        )
      ),
      child <-- debouncedErrorStream.map {
        case Some(err) => span(cls("-error"), "Error: " + err)
        case None => span(cls("-success"), "Email ok!")
      }
    )
  end Time








  def main(args: Array[String]): Unit =
      documentEvents.onDomContentLoaded.foreach { _ =>
        render(dom.document.getElementById("appContainer"), div(SearcBoxApp.app(FutureApi), Time.app, Time.clickApp, Client1.TodoMvcApp.node))
      }(unsafeWindowOwner)

end Client
