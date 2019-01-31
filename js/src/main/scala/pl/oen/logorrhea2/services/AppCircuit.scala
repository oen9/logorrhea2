package pl.oen.logorrhea2.services

import cats.data.Writer
import diode._
import diode.react.ReactConnector
import cats.implicits._

sealed trait LogMsgStatus

case object LogOk extends LogMsgStatus

case object LogError extends LogMsgStatus

case class LogMsg(msg: String, status: LogMsgStatus)

case class User(id: Int, name: String = "unknown", lastMsg: String = "")

case class ChatMsg(user: User, msg: String)

case class RoomData(roomName: String, users: Vector[User] = Vector.empty, msgs: Vector[ChatMsg] = Vector.empty)

case class Root(me: Option[User] = None,
                logs: List[LogMsg] = List(AppCircuit.logMsgError("dummy error"), AppCircuit.logMsgOk("connecting ...")),
                roomData: Option[RoomData] = None,
                rooms: Vector[String] = Vector("general", "funny", "serious"),
                savedRooms: Vector[RoomData] = Vector.empty)

case class RootModel(root: Root)

case object ExitRoom extends Action

case class EnterRoom(roomName: String) extends Action

case class SendMsg(msg: String) extends Action

case class ChangeMyName(name: String) extends Action

case class CreateNewRoom(name: String) extends Action

class ClicksHandler[M](modelRW: ModelRW[M, Root]) extends ActionHandler(modelRW) {

  val dummyMe = User(39749)

  def dummyRoom(roomName: String) =
    RoomData(
      roomName = roomName,
      users = Vector(
        User(75380, "user1", "hey"),
        User(869447),
        User(4338, "user2", "Hello!")
      ),
      msgs = Vector.empty
    )

  def updateSaved(saved: Vector[RoomData], curr: RoomData): Vector[RoomData] = {
    saved.filterNot(r => r.roomName == curr.roomName) :+ curr.copy(users = curr.users.drop(1))
  }

  override def handle = {
    case ExitRoom =>
      val saved = value.roomData.fold(value.savedRooms)(r => updateSaved(value.savedRooms, r))
      updated(value.copy(savedRooms = saved, roomData = None))

    case EnterRoom(roomName) =>
      import AppCircuit._
      val me = value.me.fold(logWriterError("reading dummy me", dummyMe) )(oldMe => logWriterOk("applying old me", oldMe)).run
      val room = value.savedRooms.find(_.roomName == roomName).getOrElse(dummyRoom(roomName))
      val roomWithMe = room.copy(users = me._2 +: room.users)

      val saved = value.roomData.fold(value.savedRooms)(r => updateSaved(value.savedRooms, r))
      updated(value.copy(me = Some(me._2), roomData = Some(roomWithMe), savedRooms = saved, logs = me._1 ++ value.logs))

    case SendMsg(msg) =>
      (value.me, value.roomData).bisequence.fold(ActionResult.NoChange: ActionResult[M]) { case (me, roomData) =>
          val newRoomData = roomData.copy(msgs = roomData.msgs :+ ChatMsg(me, msg))
          updated(value.copy(roomData = Some(newRoomData)))
      }
    case ChangeMyName(newName) => updated(value.copy(me = value.me.map(u => u.copy(name = newName))))
    case CreateNewRoom(roomName) => updated(value.copy(rooms = value.rooms :+ roomName))
  }
}

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  override protected def initialModel: RootModel = {
    RootModel(Root())
  }

  override protected def actionHandler: AppCircuit.HandlerFunction = composeHandlers(
    new ClicksHandler(zoomTo(_.root))
  )

  type Logged[A] = Writer[List[LogMsg], A]

  def logMsgOk(s: String) = LogMsg(s, LogOk)

  def logMsgError(s: String) = LogMsg(s, LogError)

  def logWriterOk[A](s: String, value: A): Logged[A] = Writer(List(LogMsg(s, LogOk)), value)

  def logWriterError[A](s: String, value: A): Logged[A] = Writer(List(LogMsg(s, LogError)), value)
}
