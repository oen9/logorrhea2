package pl.oen.logorrhea2.modules

import cats.implicits._
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import pl.oen.logorrhea2.Logorrhea2Main.{Loc, NewRoomLoc, RoomLoc}
import pl.oen.logorrhea2.services.AppData._
import pl.oen.logorrhea2.shared.User

import scala.scalajs.js.URIUtils

object Room {

  case class Props(roomName: String, proxy: ModelProxy[Root], router: RouterCtl[Loc])

  case class State(roomName: String, newMsg: String, newRoomName: Option[String] = None)

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

    def changeRoomName(e: ReactEvent): Callback = {
      e.preventDefault()

      for {
        s <- $.state
        p <- $.props
        decodedRoomName = URIUtils.decodeURI(s.roomName)
        _ <- s.newRoomName.fold(
          $.modState(_.copy(newRoomName = decodedRoomName.some))
        )(newRoomName => {
          val encodedNewRoomName = URIUtils.encodeURI(newRoomName)
          $.modState(_.copy(newRoomName = None)) >> p.proxy.dispatchCB(ChangeCurrentRoomName(encodedNewRoomName))
        })
      } yield ()
    }

    def removeRoom(e: ReactEvent): Callback = {
      e.preventDefault()

      for {
        p <- $.props
        _ <- p.proxy.dispatchCB(RemoveCurrentRoom)
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
      _ <- p.proxy.value.roomName.fold(p.router.set(NewRoomLoc))(realRoomName =>
        if (realRoomName != s.roomName) p.router.set(RoomLoc(realRoomName))
        else Callback.empty
      )
      _ <- scrollChat()
    } yield ()

    private val chatDiv = <.div(^.cls := "email-content-body-scrolled")
    private val chatDivRef = Ref[html.Div]

    def scrollChat(): Callback = chatDivRef.foreach(chatD => {
      chatD.scrollTop = chatD.scrollHeight
    })

    def updateNewRoomName(e: ReactEventFromInput): Callback = {
      val newValue = e.target.value
      $.modState(_.copy(newRoomName = Some(newValue)))
    }

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
      val decodedRoomName = URIUtils.decodeURI(props.roomName)

      <.div(^.cls := "email-content-scrolled",
        <.div(^.cls := "email-content-header pure-g",
          <.form(^.cls := "pure-form", ^.width := "100%",
            <.div(^.cls := "pure-u-1-2",
              <.h1(^.cls := "room-content-title",
                state.newRoomName.fold(<.span(decodedRoomName))(newRoomName =>
                  <.span(<.input.text(^.cls := "pure-input", ^.width := "100%", ^.value := newRoomName, ^.onChange ==> updateNewRoomName))
                )
              )
            ),

            <.div(^.cls := "email-content-controls pure-u-1-2",
              <.button(^.cls := "secondary-button pure-button", "change room name", ^.onClick ==> changeRoomName),
              <.button(^.cls := "secondary-button pure-button", "remove room", ^.onClick ==> removeRoom)
            )
          )
        ),

        chatDiv.withRef(chatDivRef)(
          (props.proxy.value.roomData, props.proxy.value.me).bisequence
            .fold(
              VdomArray("joining room in progress ...")
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

  def apply(roomName: String, router: RouterCtl[Loc])(proxy: ModelProxy[Root]) = component(Props(roomName, proxy, router))
}
