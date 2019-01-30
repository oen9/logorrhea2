package pl.oen.logorrhea2.services

import cats.data.Writer
import diode._
import diode.react.ReactConnector
import pl.oen.logorrhea2.services.AppCircuit.Logged

sealed trait LogMsgStatus
case object LogOk extends LogMsgStatus
case object LogError extends LogMsgStatus
case class LogMsg(msg: String, status: LogMsgStatus)

case class User(id: Int, name: String = "unknown", lastMsg: String = "")
case class ChatMsg(user: User, msg: String)
case class RoomData(users: Vector[User], msgs: Vector[ChatMsg])
case class Root(me: Option[User] = None,
                logs: List[LogMsg] = List(AppCircuit.logMsgError("dummy error"), AppCircuit.logMsgOk("connecting ...")),
                users: Vector[User] = Vector.empty,
                msgs: Vector[String] = Vector.empty,
                roomData: Option[RoomData] = None,
                rooms: Vector[String] = Vector("general", "funny", "serious"))
case class RootModel(root: Root)

case object ClearUsers extends Action
case object PutDummyUsers extends Action
case class SendMsg(msg: String) extends Action
case class ChangeMyName(name: String) extends Action
case class CreateNewRoom(name: String) extends Action

class ClicksHandler[M](modelRW: ModelRW[M, Root]) extends ActionHandler(modelRW) {

  val dummyMe = User(39749)

  val dummyUsers = Vector(
    User(75380, "user1", "hey"),
    User(869447),
    User(4338, "user2", "Hello!")
  )

  override def handle = {
    case ClearUsers => updated(value.copy(users = Vector.empty))
    case PutDummyUsers =>
      val newMeWrt: Logged[User] = value.me.fold(AppCircuit.logWriterError("reading dummy me", dummyMe))(oldMe => AppCircuit.logWriterOk("applying old me", oldMe))
      val newMe = newMeWrt.run
      updated(value.copy(users = dummyUsers, me = Some(newMe._2), logs = newMe._1 ++ value.logs))
    case SendMsg(msg) => updated(value.copy(msgs = value.msgs :+ msg))
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
