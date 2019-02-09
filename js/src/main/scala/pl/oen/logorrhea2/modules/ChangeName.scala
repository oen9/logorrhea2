package pl.oen.logorrhea2.modules

import cats.implicits._
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import pl.oen.logorrhea2.services.AppData.{ChangeMyName, Root}

object ChangeName {

  case class Props(proxy: ModelProxy[Root])

  case class State(newName: Option[String])

  class Backend($: BackendScope[Props, State]) {
    def updateNewName(e: ReactEventFromInput): Callback = {
      val newValue = e.target.value
      $.setState(State(Some(newValue)))
    }

    def accept(e: ReactEventFromInput): Callback = {
      e.preventDefault()

      for {
        s <- $.state
        p <- $.props
        nameToChange = s.newName.filter(_.nonEmpty)
        _ <- nameToChange.fold(Callback.empty)(name => p.proxy.dispatchCB(ChangeMyName(name)))
      } yield ()
    }

    def onUpdate(): Callback = {
      def initName(s: String): Callback = {
        $.modState(_.copy(newName = Some(s)))
      }

      for {
        props <- $.props
        state <- $.state
        nameFromMe = props.proxy.value.me.map(_.name)
        _ <- state.newName.map(_ => Callback.empty)
          .orElse(nameFromMe.map(initName))
          .getOrElse(Callback.empty)
      } yield ()
    }

    def render(props: Props, state: State) =
      <.div(^.cls := "email-content",
        <.div(^.cls := "email-content-header pure-g",
          <.div(^.cls := "pure-u-1-2",
            <.h1(^.cls := "email-content-title", "change name"),
          ),
        ),
        (props.proxy.value.me, state.newName).bisequence.fold(
          <.div("connection error (please wait or refresh page)")
        ) { case (me, newName) =>

          <.div(^.cls := "email-content-body",
            <.div(^.cls := "pure-g odd-row",
              <.div(^.cls := "pure-u-1-5", "id"),
              <.div(^.cls := "pure-u-1-5"),
              <.div(^.cls := "pure-u-1-5", me.id)
            ),

            <.form(^.cls := "pure-form",
              <.div(^.cls := "pure-g even-row",
                <.div(^.cls := "pure-u-1-5", <.span(^.cls := "form-vcenter", "name")),
                <.div(^.cls := "pure-u-1-5"),
                <.div(^.cls := "pure-u-1-5",
                  <.input.text(^.cls := "pure-input", ^.width := "100%", ^.value := newName, ^.onChange ==> updateNewName, ^.autoFocus := true)
                ),
                <.div(^.cls := "pure-u-1-5"),
                <.div(^.cls := "pure-u-1-5",
                  <.button(^.cls := "primary-button pure-button", ^.width := "100%", ^.onClick ==> accept, "accept")
                )
              )
            ),

            <.div(^.cls := "pure-g odd-row",
              <.div(^.cls := "pure-u-1-5", "last msg"),
              <.div(^.cls := "pure-u-1-5"),
              <.div(^.cls := "pure-u-1-5", "")
            )
          )
        },
      )
  }

  val component = ScalaComponent.builder[Props]("Room")
    .initialStateFromProps(props => State(props.proxy.value.me.map(_.name)))
    .renderBackend[Backend]
    .componentDidUpdate(_.backend.onUpdate())
    .build

  def apply(proxy: ModelProxy[Root]) = component(Props(proxy))
}
