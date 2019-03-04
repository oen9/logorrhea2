package pl.oen.logorrhea2.services

import cats.implicits._
import diode._
import diode.react.ReactConnector
import pl.oen.logorrhea2.services.AppData._
import pl.oen.logorrhea2.shared._
import monocle.std.option.some
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global

class ClicksHandler[M](modelRW: ModelRW[M, Root]) extends ActionHandler(modelRW) {

  override def handle = {
    case ExitRoom =>
      updated(
        value.copy(roomData = None, roomName = None),
        Websock.sendAsEffect(value.ws, AbandonRoom)
      )

    case EnterRoom(roomName) =>
      val setRoomName = Root.roomName.set(Some(roomName))
      val msg = JoinRoom(roomName)
      updated(setRoomName(value), Websock.sendAsEffect(value.ws, msg))

    case SendMsg(message) =>
      (value.me, value.roomData).bisequence.fold(ActionResult.NoChange: ActionResult[M]) { case (me, roomData) =>
        val cmd = RegisterMessage(Msg(me, message), roomData.roomName)
        effectOnly(Websock.sendAsEffect(value.ws, cmd))
      }

    case ChangeMyName(newName) =>
      val newRoot = Root.me.modify(_.map(_.copy(name = newName)))
      val msg = ChangeName(newName)
      updated(newRoot(value), Websock.sendAsEffect(value.ws, msg))

    case CreateNewRoom(roomName) =>
      effectOnly(Websock.sendAsEffect(value.ws, AddRoom(roomName)))
  }
}

class WebsockReceiverHandler[M](modelRW: ModelRW[M, Root]) extends ActionHandler(modelRW) {

  override protected def handle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateRooms(names) =>
      val updateRooms = Root.rooms.set(names)
      updated(updateRooms(value))

    case AddNewRoom(name) =>
      val addRoom = Root.rooms.modify(_ :+ name)
      updated(addRoom(value))

    case EnteredRoom(room) =>
      val log = logMsgOk(s"entered room: ${room.name}")
      val roomData = RoomData(room.name, room.users, room.msgs)

      val setLogs = Root.logs.modify(log +: _)
      val setRoomData = Root.roomData.set(Some(roomData))
      updated((setLogs compose setRoomData) (value))

    case SomeoneEntered(u) =>
      val addUserToList = (Root.roomData composePrism some composeLens RoomData.users).modify(_ :+ u)
      updated(addUserToList(value))

    case SomeoneExitted(u) =>
      val removeUser = (Root.roomData composePrism some composeLens RoomData.users)
        .modify(_.filter(_.id != u.id))
      updated(removeUser(value))

    case SomeoneSentMsg(msg) =>
      val addMsg = (Root.roomData composePrism some composeLens RoomData.msgs).modify(_ :+ msg)
      updated(addMsg(value))
  }
}

class WebsockLifecycleHandler[M](modelRW: ModelRW[M, Root]) extends ActionHandler(modelRW) {

  override protected def handle: PartialFunction[Any, ActionResult[M]] = {
    case Connected(user) =>
      val setMe = Root.me.set(Some(user))
      val addLog = Root.logs.modify(logs => logMsgOk("Connected") +: logs)
      value.roomName.fold(
        updated((setMe compose addLog) (value))
      ) { roomName =>
        val msg = JoinRoom(roomName)
        updated((setMe compose addLog) (value), Websock.sendAsEffect(value.ws, msg))
      }

    case Disconnected =>
      import diode.Implicits.runAfterImpl
      val clearMe = Root.me.set(None)
      val addLog = Root.logs.modify(logs => logMsgError("Disconnected. 5 seconds until reconnect.") +: logs)
      val clearRoom = Root.roomData.set(None)
      updated((clearMe compose addLog compose clearRoom) (value), Effect.action(Connect).after(5.second))

    case Connect =>
      val addLog = Root.logs.modify(logs => logMsgOk("Connecting...") +: logs)
      val setWs = Root.ws.set(Websock.connect())
      updated((setWs compose addLog) (value))
  }
}

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  override protected def initialModel: RootModel = {
    val ws = Websock.connect()
    RootModel(Root(ws))
  }

  override protected def actionHandler: AppCircuit.HandlerFunction = composeHandlers(
    new ClicksHandler(zoomTo(_.root)),
    new WebsockReceiverHandler(zoomTo(_.root)),
    new WebsockLifecycleHandler(zoomTo(_.root))
  )
}
