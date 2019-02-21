package pl.oen.logorrhea2.services

import cats.effect.Effect
import pl.oen.logorrhea2.services.RoomService.RoomInfo
import pl.oen.logorrhea2.services.UserService.UserInfo
import pl.oen.logorrhea2.shared.{Msg, Room}

trait RoomService[F[_]] {
  def createRoom(name: String): F[Option[RoomInfo[F]]]
  def getRooms(): F[Vector[Room]]

  protected[this] def roomToData(ri: RoomInfo[F]): Room = {
    Room(ri.name, ri.users.map(_.u), ri.msgs)
  }
}

object RoomService {
  val initMsgs: Vector[Msg] = Vector(Msg(0, "SERVER", "Welcome!"))

  def apply[F[_] : Effect](): F[RoomService[F]] = RoomServiceImpl()

  case class RoomInfo[F[_]](name: String,
                            users: Vector[UserInfo[F]] = Vector.empty,
                            msgs: Vector[Msg] = initMsgs)
}
