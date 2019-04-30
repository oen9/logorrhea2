package pl.oen.logorrhea2.services

import cats.data.Writer
import diode.Action
import monocle.macros.Lenses
import org.scalajs.dom.WebSocket
import pl.oen.logorrhea2.shared.{Msg, Room, User}

import scala.scalajs.js.Date

object AppData {
  case object ExitRoom extends Action
  case class EnterRoom(roomName: String) extends Action
  case class SendMsg(msg: String) extends Action
  case class ChangeMyName(name: String) extends Action
  case class CreateNewRoom(name: String) extends Action
  case class ChangeCurrentRoomName(newRoomName: String) extends Action

  case class UpdateRooms(names: Vector[String]) extends Action
  case class AddNewRoom(name: String) extends Action
  case class EnteredRoom(room: Room) extends Action
  case class SomeoneEntered(user: User) extends Action
  case class SomeoneExitted(user: User) extends Action
  case class SomeoneSentMsg(msg: Msg) extends Action
  case class SomeoneChangedRoomName(newRoomName: String, oldRoomName: String) extends Action

  case object Connect extends Action
  case class Connected(user: User) extends Action
  case object Disconnected extends Action

  sealed trait LogMsgStatus
  case object LogOk extends LogMsgStatus
  case object LogError extends LogMsgStatus

  case class LogMsg(msg: String, status: LogMsgStatus)
  @Lenses case class RoomData(roomName: String, users: Vector[User] = Vector.empty, msgs: Vector[Msg] = Vector.empty)
  @Lenses case class Root(ws: WebSocket,
                          me: Option[User] = None,
                          logs: List[LogMsg] = List(logMsgOk("Connecting..."), logMsgOk("Application started")),
                          roomName: Option[String] = None,
                          roomData: Option[RoomData] = None,
                          rooms: Vector[String] = Vector.empty)
  case class RootModel(root: Root)

  type Logged[A] = Writer[List[LogMsg], A]
  def withTime(s: String): String = s"[${new Date().toLocaleTimeString()}]: $s"
  def logMsgOk(s: String) = LogMsg(withTime(s), LogOk)
  def logMsgError(s: String) = LogMsg(withTime(s), LogError)
  def logWriterOk[A](s: String, value: A): Logged[A] = Writer(List(logMsgOk(s)), value)
  def logWriterError[A](s: String, value: A): Logged[A] = Writer(List(logMsgError(s)), value)
}
