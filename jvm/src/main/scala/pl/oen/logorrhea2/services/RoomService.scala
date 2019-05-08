package pl.oen.logorrhea2.services

import cats.effect.Effect
import pl.oen.logorrhea2.services.RoomService.RoomInfo
import pl.oen.logorrhea2.services.UserService.UserInfo
import pl.oen.logorrhea2.shared.{JoinRoom, Msg, Room, User}

trait RoomService[F[_]] {
  def createRoom(name: String): F[Option[RoomInfo[F]]]
  def getRooms(): F[Vector[Room]]
  def joinRoom(u: UserInfo[F], jr: JoinRoom): F[Option[RoomInfo[F]]]
  def abandonRoom(u: UserInfo[F]): F[Option[RoomInfo[F]]]
  def registerMessage(msg: Msg, roomName: String): F[Option[RoomInfo[F]]]
  def changeRoomName(newRoomName: String, oldRoomName: String): F[Option[RoomInfo[F]]]
  def removeRoom(roomName: String): F[Option[RoomInfo[F]]]

  protected[this] def roomToData(ri: RoomInfo[F]): Room = {
    Room(ri.name, ri.users.map(_.u), ri.msgs)
  }
}

object RoomService {
  val serverUser = User(0, "SERVER")
  val initMsgs: Vector[Msg] = Vector(Msg(serverUser, "Welcome!"))

  def apply[F[_] : Effect](mongoService: MongoService[F]): F[RoomService[F]] = RoomServiceImpl(mongoService)

  case class RoomInfo[F[_]](name: String,
                            users: Vector[UserInfo[F]] = Vector.empty,
                            msgs: Vector[Msg] = initMsgs)

  def roomToData[F[_]](ri: RoomInfo[F]): Room = Room(ri.name, ri.users.map(_.u), ri.msgs)
}
