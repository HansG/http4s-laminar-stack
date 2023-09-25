package example.frontend

import com.raquo.laminar.api.L.*
import com.raquo.laminar.api.L.given
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


  object TodoMvcApp :

    // This implementation is very loosely based on Outwatch TodoMVC, for comparison see
    // https://github.com/clovellytech/outwatch-examples/tree/master/todomvc/src/main/scala/todomvc


    // --- Models ---

    case class TodoItem(id: Int, text: String, completed: Boolean)


    enum Filter(val name: String, val passes: TodoItem => Boolean):
      case ShowAll extends Filter("All", _ => true)
      case ShowActive extends Filter("Active", !_.completed)
      case ShowCompleted extends Filter("Completed", _.completed)

    import Filter.*

    val filters  = Filter.values // ShowAll :: ShowActive :: ShowCompleted :: Nil


    trait Command
    case class Create(itemText: String) extends Command
    case class  UpdateText(itemId: Int, text: String) extends Command
    case  class  UpdateCompleted(itemId: Int, completed: Boolean) extends Command
    case  class  Delete(itemId: Int) extends Command
    case  object  DeleteCompleted extends Command

    //import Command.*

    // --- State ---

    // Var-s are reactive state variables suitable for both local state and redux-like global stores.
    // Laminar uses my library Airstream as its reactive layer https://github.com/raquo/Airstream

    private val itemsVar = Var(List[TodoItem]())
    private val filterVar = Var[Filter](ShowAll)
    private var lastId = 1 // just for auto-incrementing IDs

    private val commandObserver = Observer[Command] {
      case Create(itemText)  =>
        lastId += 1
        if (filterVar.now() == ShowCompleted)
          filterVar.set(ShowAll)
        itemsVar.update(_ :+ TodoItem(id = lastId, text = itemText, completed = false))
      case UpdateText(itemId, text) =>
        itemsVar.update(_.map(item => if (item.id == itemId) item.copy(text = text) else item))
      case UpdateCompleted(itemId, completed) =>
        itemsVar.update(_.map(item => if (item.id == itemId) item.copy(completed = completed) else item))
      case Delete(itemId) =>
        itemsVar.update(_.filterNot(_.id == itemId))
      case DeleteCompleted =>
        itemsVar.update(_.filterNot(_.completed))
    }


    // --- Views ---

    lazy val node: HtmlElement = {
      val todoItemsSignal0 = itemsVar
        .signal
        .combineWith(filterVar.signal)
      val todoItemsSignal =  todoItemsSignal0.mapN((li, f) =>  li filter f.passes)
      div(
        cls("todoapp"),
        div(
          cls("header"),
          h1("todos"),
          renderNewTodoInput,
        ),
        div(
          hideIfNoItems,
          cls("main"),
          ul(
            cls("todo-list"),
            children <-- todoItemsSignal.split(_.id)(renderTodoItem)
          )
        ),
        renderStatusBar
      )
    }

    private def renderNewTodoInput =
      input(
        cls("new-todo"),
        placeholder("What needs to be done?"),
        autoFocus(true),
        onEnterPress
          .mapToValue
          .filter(_.nonEmpty)
          .map(Create(_)) --> commandObserver
//          .setAsValue("")
      )

    // Render a single item. Note that the result is a single element: not a stream, not some virtual DOM representation.
    private def renderTodoItem(itemId: Int, initialTodo: TodoItem, itemSignal: Signal[TodoItem]): HtmlElement = {
      val isEditingVar = Var(false) // Example of local state
      val updateTextObserver = commandObserver.contramap[UpdateText] { updateCommand =>
        isEditingVar.set(false)
        updateCommand
      }
      li(
        cls <-- itemSignal.map(item => Map("completed" -> item.completed)),
        onDblClick.filter(_ => !isEditingVar.now()).mapTo(true) --> isEditingVar.writer,
        children <-- isEditingVar.signal.map[List[HtmlElement]] {
          case true =>
            renderTextUpdateInput(itemId, itemSignal, updateTextObserver) :: Nil
          case false =>
            List(
              renderCheckboxInput(itemId, itemSignal),
              label(child.text <-- itemSignal.map(_.text)),
              button(
                cls("destroy"),
                onClick.mapTo(Delete(itemId)) --> commandObserver
              )
            )
        }
      )
    }

    // Note that we pass reactive variables: `itemSignal` for reading, `updateTextObserver` for writing
    private def renderTextUpdateInput(
                                       itemId: Int,
                                       itemSignal: Signal[TodoItem],
                                       updateTextObserver: Observer[UpdateText]
                                     ) =
      input(
        cls("edit"),
        defaultValue <-- itemSignal.map(_.text),
        onMountFocus,
        onEnterPress.mapToValue.map(UpdateText(itemId, _)) --> updateTextObserver,
        onBlur.mapToValue.map(UpdateText(itemId, _)) --> updateTextObserver
      )

    private def renderCheckboxInput(itemId: Int, itemSignal: Signal[TodoItem]) =
      input(
        cls("toggle"),
        typ("checkbox"),
        checked <-- itemSignal.map(_.completed),
        onInput.mapToChecked.map { isChecked =>
          UpdateCompleted(itemId, completed = isChecked)
        } --> commandObserver
      )

    private def renderStatusBar =
      footerTag(
        hideIfNoItems,
        cls("footer"),
        span(
          cls("todo-count"),
          child.text <-- itemsVar.signal
            .map(_.count(!_.completed))
            .map(pluralize(_, "item left", "items left")),
        ),
        ul(
          cls("filters"),
          filters.map(filter => li(renderFilterButton(filter)))
        ),
        child.maybe <-- itemsVar.signal.map { items =>
          if (items.exists(ShowCompleted.passes)) Some(
            button(
              cls("clear-completed"),
              "Clear completed",
              onClick.map(_ => DeleteCompleted) --> commandObserver
            )
          ) else None
        }
      )

    private def renderFilterButton(filter: Filter) =
      a(
        cls.toggle("selected") <-- filterVar.signal.map(_ == filter),
        onClick.preventDefault.mapTo(filter) --> filterVar.writer,
        filter.name
      )

    // Every little thing in Laminar can be abstracted away
    private def hideIfNoItems: Mod[HtmlElement] =
      display <-- itemsVar.signal.map { items =>
        if (items.nonEmpty) "" else "none"
      }


    // --- Generic helpers ---

    private def pluralize(num: Int, singular: String, plural: String): String =
      s"$num ${if (num == 1) singular else plural}"

    private val onEnterPress = onKeyPress.filter(_.keyCode == dom.ext.KeyCode.Enter)

  end TodoMvcApp







  def main(args: Array[String]): Unit =
        documentEvents.onDomContentLoaded.foreach { _ =>
          render(dom.document.getElementById("appContainer"), div(SearchApp(FutureApi), HelloWorld.rootElement, Time.timeApp, Time.clickApp)  )
        }(unsafeWindowOwner)



end Client
