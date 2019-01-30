package pl.oen.logorrhea2.modules

import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import pl.oen.logorrhea2.services._

import scala.scalajs.js.URIUtils

object Room {

  case class Props(roomName: String, proxy: ModelProxy[Root])

  case class State(newMsg: String = "")

  class Backend($: BackendScope[Props, State]) {
    def send(e: ReactEvent): Callback = {
      e.preventDefault()

      val sendMsg = for {
        s <- $.state
        p <- $.props
        msg = if (s.newMsg.nonEmpty) Some(s.newMsg) else None
        _ <- msg.fold(Callback.empty)(m => p.proxy.dispatchCB(SendMsg(m)))
      } yield ()

      sendMsg >> $.modState(_ => State())
    }

    def connect(): Callback = $.props.flatMap(_.proxy.dispatchCB(PutDummyUsers))

    def disconnect(): Callback = $.props.flatMap(_.proxy.dispatchCB(ClearUsers))

    def mount(): Callback = connect() >> scrollChat()

    def umount(): Callback = disconnect()

    private val chatDiv = <.div(^.cls := "email-content-body-scrolled")
    private val chatDivRef = Ref[html.Div]

    def scrollChat(): Callback = chatDivRef.foreach(chatD => {
      chatD.scrollTop = chatD.scrollHeight
    })

    def updateNewMsg(e: ReactEventFromInput): Callback = {
      val newValue = e.target.value
      $.modState(_.copy(newValue))
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
          (for (m <- props.proxy.value.msgs.zipWithIndex) yield {
            val colourClass = if (m._2 % 2 == 1) "odd-row" else "even-row"
            <.div(^.key := m._2, ^.cls := colourClass, m._1)
          }).toVdomArray
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
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(_.backend.mount())
    .componentWillUnmount(_.backend.umount())
    .componentDidUpdate(_.backend.scrollChat())
    .build

  def apply(roomName: String)(proxy: ModelProxy[Root]) = component(Props(roomName, proxy))
}
