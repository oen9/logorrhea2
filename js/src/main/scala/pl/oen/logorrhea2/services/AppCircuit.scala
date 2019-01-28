package pl.oen.logorrhea2.services

import diode.{Action, ActionHandler, Circuit, ModelRW}
import diode.react.ReactConnector

case class User(id: Int, name: String = "unknown", lastMsg: String = "")
case class Root(users: Vector[User] = Vector.empty, msgs: Vector[String] = Vector.empty)
case class RootModel(root: Root)

case object ClearUsers extends Action
case object PutDummyUsers extends Action
case class SendMsg(msg: String) extends Action

class ClicksHandler[M](modelRW: ModelRW[M, Root]) extends ActionHandler(modelRW) {

  val dummyUsers = Vector(
    User(75380, "user1", "hey"),
    User(869447),
    User(4338, "user2", "Hello!")
  )

  override def handle = {
    case ClearUsers => updated(value.copy(users = Vector.empty))
    case PutDummyUsers => updated(value.copy(users = dummyUsers))
    case SendMsg(msg) => updated(value.copy(msgs = value.msgs :+ msg))
  }
}

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  override protected def initialModel: RootModel = {
    RootModel(Root())
  }

  override protected def actionHandler: AppCircuit.HandlerFunction = composeHandlers(
    new ClicksHandler(zoomTo(_.root))
  )
}
