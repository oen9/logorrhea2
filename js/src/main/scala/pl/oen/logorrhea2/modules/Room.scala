package pl.oen.logorrhea2.modules

import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import pl.oen.logorrhea2.services._
import cats.implicits._

import scala.scalajs.js.URIUtils

object Room {

  case class Props(roomName: String, proxy: ModelProxy[Root])

  case class State(roomName: String, newMsg: String)

  class Backend($: BackendScope[Props, State]) {
    def send(e: ReactEvent): Callback = {
      e.preventDefault()

      for {
        s <- $.state
        p <- $.props
        msg = if (s.newMsg.nonEmpty) Some(s.newMsg) else None
        _ <- msg.fold(Callback.empty)(m => p.proxy.dispatchCB(SendMsg(m)))
        _ <- $.modState(_.copy(newMsg = ""))
      } yield ()
    }

    def enterRoom(): Callback = {
      for {
        p <- $.props
        _ <- p.proxy.dispatchCB(EnterRoom(p.roomName))
        _ <- $.modState(s => s.copy(roomName = p.roomName))
      } yield ()
    }

    def disconnect(): Callback = $.props.flatMap(_.proxy.dispatchCB(ExitRoom))

    def mount(): Callback = enterRoom() >> scrollChat()

    def umount(): Callback = disconnect()

    def onUpdate() = for {
      s <- $.state
      p <- $.props
      _ <- if (s.roomName == p.roomName) Callback.empty else enterRoom()
      _ <- scrollChat()
    } yield ()

    private val chatDiv = <.div(^.cls := "email-content-body-scrolled")
    private val chatDivRef = Ref[html.Div]

    def scrollChat(): Callback = chatDivRef.foreach(chatD => {
      chatD.scrollTop = chatD.scrollHeight
    })

    def updateNewMsg(e: ReactEventFromInput): Callback = {
      val newValue = e.target.value
      $.modState(_.copy(newMsg = newValue))
    }

    def chatContentList(room: RoomData, me: User): VdomArray = {
      room.msgs.zipWithIndex.map { case (chatMsg, index) =>
        val colourClass = if (index % 2 == 1) "odd-row" else "even-row"
        <.div(^.key := index, ^.cls := colourClass,
          <.span(^.cls := "chat-msg-id", s"(${chatMsg.user.id})"),
          <.span(
            ^.cls := "chat-msg-name",
            (^.cls := "chat-msg-name-me").when(chatMsg.user.id == me.id),
            s"${chatMsg.user.name}"),
          <.span(^.cls := "chat-msg-content", s"${chatMsg.msg}")
        )
      }.toVdomArray
    }

    def render(props: Props, state: State) = {
      val decodedRoomname = URIUtils.decodeURI(props.roomName)

      <.div(^.cls := "email-content-scrolled",
        <.div(^.cls := "email-content-header pure-g",
          <.div(^.cls := "pure-u-1-2",
            <.h1(^.cls := "email-content-title", decodedRoomname),
          ),

          <.div(^.cls := "email-content-controls pure-u-1-2",
            <.button(^.cls := "secondary-button pure-button", "change room name"),
            <.button(^.cls := "secondary-button pure-button", "remove room")
          )
        ),

        chatDiv.withRef(chatDivRef)(
          (props.proxy.value.roomData, props.proxy.value.me).bisequence
            .fold(
              VdomArray("connection error (please wait or refresh page)")
            ) { case (room, me) =>
              chatContentList(room, me)
            }
        ),

        <.div(^.cls := "email-content-footer",
          <.form(^.cls := "pure-form",
            <.div(^.cls := "pure-g",
              <.div(^.cls := "pure-u-3-4",
                <.input.text(^.cls := "pure-input", ^.width := "100%", ^.value := state.newMsg, ^.onChange ==> updateNewMsg, ^.autoFocus := true)
              ),
              <.div(^.cls := "pure-u-1-4",
                <.button(^.cls := "primary-button pure-button", ^.width := "100%", ^.onClick ==> send, "Send")
              )
            )
          )
        )
      )
    }
  }

  val component = ScalaComponent.builder[Props]("Room")
    .initialState(State("", ""))
    .renderBackend[Backend]
    .componentDidMount(_.backend.mount())
    .componentWillUnmount(_.backend.umount())
    .componentDidUpdate(_.backend.onUpdate())
    .build

  def apply(roomName: String)(proxy: ModelProxy[Root]) = component(Props(roomName, proxy))
}
