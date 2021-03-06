package pl.oen.logorrhea2.services

import diode.{Effect, NoAction}
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.scalajs.dom
import org.scalajs.dom.{CloseEvent, Event, MessageEvent, WebSocket}
import pl.oen.logorrhea2.services.AppData._
import pl.oen.logorrhea2.shared._

import scala.concurrent.ExecutionContext
import scala.scalajs.js

object Websock {

  val protocol = if ("http:" == dom.window.location.protocol) "ws://" else "wss://"
  val url = protocol + dom.window.location.host + "/chat"

  def connect() = {
    def onopen(e: Event): Unit = { }

    def onmessage(e: MessageEvent): Unit = {
      decode[Data](e.data.toString).fold(e => println(s"error: $e"), {
        case u: User => AppCircuit.dispatch(Connected(u))
        case RoomsNames(names) => AppCircuit.dispatch(UpdateRooms(names))
        case RoomAdded(name) => AppCircuit.dispatch(AddNewRoom(name))
        case JoinedRoom(room) => AppCircuit.dispatch(EnteredRoom(room))
        case SomeoneJoinedRoom(u) => AppCircuit.dispatch(SomeoneEntered(u))
        case SomeoneAbandonedRoom(u) => AppCircuit.dispatch(SomeoneExitted(u))
        case MessageRegistered(msg) => AppCircuit.dispatch(SomeoneSentMsg(msg))
        case RoomNameChanged(newRoomname, oldRoomName) => AppCircuit.dispatch(SomeoneChangedRoomName(newRoomname, oldRoomName))
        case RoomRemoved(roomName) => AppCircuit.dispatch(SomeoneRemovedRoom(roomName))
        case unknown => println(s"[ws] unsupported data: $unknown")
      })
    }

    def onerror(e: Event): Unit = {
      val msg: String = e.asInstanceOf[js.Dynamic]
        .message.asInstanceOf[js.UndefOr[String]]
        .fold(s"error occurred!")("error occurred: " + _)
      println(s"[ws] $msg")
    }

    def onclose(e: CloseEvent): Unit = {
      AppCircuit.dispatch(Disconnected)
    }

    val ws = new WebSocket(url)
    ws.onopen = onopen _
    ws.onclose = onclose _
    ws.onmessage = onmessage _
    ws.onerror = onerror _
    ws
  }

  def send(ws: dom.WebSocket, data: Data): Unit = {
    if (ws.readyState == 1) {
      val msg = data.asJson.noSpaces
      ws.send(msg)
    }
  }

  def sendAsEffect(ws: dom.WebSocket, data: Data)(implicit ec: ExecutionContext): Effect = Effect.action {
    send(ws, data)
    NoAction
  }
}
