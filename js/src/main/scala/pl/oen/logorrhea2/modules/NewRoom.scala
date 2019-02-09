package pl.oen.logorrhea2.modules

import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import pl.oen.logorrhea2.services.AppData.CreateNewRoom
import pl.oen.logorrhea2.shared.User

import scala.scalajs.js.URIUtils

object NewRoom {

  case class Props(proxy: ModelProxy[Option[User]])

  case class State(roomName: String = "")

  class Backend($: BackendScope[Props, State]) {
    def updateRoomName(e: ReactEventFromInput): Callback = {
      val newValue = e.target.value
      $.setState(State(roomName = newValue))
    }

    def accept(e: ReactEventFromInput): Callback = {
      e.preventDefault()

      val newRoomRq = for {
        s <- $.state
        p <- $.props
        newRoomName = if (s.roomName.nonEmpty) Some(s.roomName) else None
        encodedRoomName = newRoomName.map(URIUtils.encodeURI)
        _ <- encodedRoomName.fold(Callback.empty)(roomName => p.proxy.dispatchCB(CreateNewRoom(roomName)))
      } yield ()

      newRoomRq >> $.modState(s => s.copy(roomName = ""))
    }

    def render(props: Props, state: State) =
      <.div(^.cls := "email-content",
        <.div(^.cls := "email-content-header pure-g",
          <.div(^.cls := "pure-u-1-2",
            <.h1(^.cls := "email-content-title", "create new room"),
          ),
        ),
        props.proxy.value.fold(
          <.div("connection error (please wait or refresh page)")
        ) { _ =>
          <.div(^.cls := "email-content-body",
            <.form(^.cls := "pure-form",
              <.div(^.cls := "pure-g odd-row",
                <.div(^.cls := "pure-u-1-5", <.span(^.cls := "form-vcenter", "new room name")),
                <.div(^.cls := "pure-u-1-5"),
                <.div(^.cls := "pure-u-1-5",
                  <.input.text(^.cls := "pure-input", ^.width := "100%", ^.value := state.roomName, ^.onChange ==> updateRoomName, ^.autoFocus := true)
                ),
                <.div(^.cls := "pure-u-1-5"),
                <.div(^.cls := "pure-u-1-5",
                  <.button(^.cls := "primary-button pure-button", ^.width := "100%", ^.onClick ==> accept, "accept")
                )
              )
            )
          )
        },
      )
  }

  val component = ScalaComponent.builder[Props]("Room")
    .initialStateFromProps(props => State())
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[Option[User]]) = component(Props(proxy))
}
