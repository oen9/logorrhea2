package pl.oen.logorrhea2.services

import cats.implicits._
import diode._
import diode.react.ReactConnector
import pl.oen.logorrhea2.services.AppData._
import pl.oen.logorrhea2.shared.{ChangeName, User}

import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global

class ClicksHandler[M](modelRW: ModelRW[M, Root]) extends ActionHandler(modelRW) {

  val dummyMe = User(39749)

  def dummyRoom(roomName: String) =
    RoomData(
      roomName = roomName,
      users = Vector(
        User(75380, "user1"),
        User(869447),
        User(4338, "user2")
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
      val loggedMe = value.me.fold(logWriterError("reading dummy me", dummyMe))(oldMe => logWriterOk("applying old me", oldMe)).run
      val room = value.savedRooms.find(_.roomName == roomName).getOrElse(dummyRoom(roomName))
      val roomWithMe = room.copy(users = loggedMe._2 +: room.users)
      val saved = value.roomData.fold(value.savedRooms)(r => updateSaved(value.savedRooms, r))

      val setMe = Root.me.set(Some(loggedMe._2))
      val setRoomData = Root.roomData.set(Some(roomWithMe))
      val setSavedRooms = Root.savedRooms.set(saved)
      val setLogs = Root.logs.modify(loggedMe._1 ++ _)
      updated((setMe compose setRoomData compose setSavedRooms compose setLogs)(value))

    case SendMsg(msg) =>
      (value.me, value.roomData).bisequence.fold(ActionResult.NoChange: ActionResult[M]) { case (me, roomData) =>
        val newRoomData = roomData.copy(msgs = roomData.msgs :+ ChatMsg(me, msg))
        val setRoomData = Root.roomData.set(Some(newRoomData))
        updated(setRoomData(value))
      }
    case ChangeMyName(newName) =>
      val newRoot = Root.me.modify(_.map(_.copy(name = newName)))
      val msg = ChangeName(newName)
      updated(newRoot(value), Websock.sendAsEffect(value.ws, msg))

    case CreateNewRoom(roomName) =>
      val setRooms = Root.rooms.modify(_ :+ roomName)
      updated(setRooms(value))

    case Connected(user) =>
      val setMe = Root.me.set(Some(user))
      val addLog = Root.logs.modify(logs => logMsgOk("Connected") +: logs)
      updated((setMe compose addLog)(value))

    case Disconnected =>
      import diode.Implicits.runAfterImpl
      val clearMe = Root.me.set(None)
      val addLog = Root.logs.modify(logs => logMsgError("Disconnected. 5 seconds until reconnect.") +: logs)
      val clearRoom = Root.roomData.set(None)
      updated((clearMe compose addLog compose clearRoom)(value), Effect.action(Connect).after(5.second))

    case Connect =>
      val addLog = Root.logs.modify(logs => logMsgOk("Connecting...") +: logs)
      val setWs = Root.ws.set(Websock.connect())
      updated((setWs compose addLog)(value))
  }
}

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  override protected def initialModel: RootModel = {
    val ws = Websock.connect()
    RootModel(Root(ws))
  }

  override protected def actionHandler: AppCircuit.HandlerFunction = composeHandlers(
    new ClicksHandler(zoomTo(_.root))
  )
}
