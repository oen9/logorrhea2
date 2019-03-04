package pl.oen.logorrhea2.services

import cats.effect.Effect
import cats.effect.concurrent.Ref
import cats.implicits._
import pl.oen.logorrhea2.services.RoomService.RoomInfo
import pl.oen.logorrhea2.services.UserService.UserInfo
import pl.oen.logorrhea2.shared.{JoinRoom, Msg, Room}

class RoomServiceImpl[F[_] : Effect](rooms: Ref[F, Vector[RoomInfo[F]]]) extends RoomService[F] {

  override def createRoom(name: String): F[Option[RoomInfo[F]]] = for {
    result <- rooms.modify(ri => addRoom(ri, name))
  } yield result

  override def getRooms(): F[Vector[Room]] = for {
    rms <- rooms.get
    roomsNames = rms.map(roomToData)
  } yield roomsNames

  override def joinRoom(u: UserInfo[F], jr: JoinRoom): F[Option[RoomInfo[F]]] =
    rooms.modify(rs => addUserToRoom(u, jr, rs))

  override def abandonRoom(u: UserInfo[F]): F[Option[RoomInfo[F]]] = {
    def modify(rv: Vector[RoomInfo[F]], roomName: String): (Vector[RoomInfo[F]], Option[RoomInfo[F]]) = {
      val updatedRv = rv.map { r =>
        if (r.name != roomName) r
        else {
          val updatedUsers = r.users.filter(_.u.id != u.u.id)
          r.copy(users = updatedUsers)
        }
      }

      (updatedRv, updatedRv.find(_.name == roomName))
    }

    u.room.fold(Effect[F].pure(none[RoomInfo[F]]))(roomName => rooms.modify(modify(_, roomName)))
  }

  override def registerMessage(msg: Msg, roomName: String): F[Option[RoomInfo[F]]] = rooms.modify(addMessage(msg, roomName, _))

  private[this] def addRoom(ris: Vector[RoomInfo[F]], name: String): (Vector[RoomInfo[F]], Option[RoomInfo[F]]) = {
    def newRoom: (Vector[RoomInfo[F]], Option[RoomInfo[F]]) = {
      val created = RoomInfo[F](name)
      (ris :+ created, Some(created))
    }

    ris.find(_.name == name)
      .fold(newRoom)(_ => (ris, None))
  }

  private[this] def addUserToRoom(u: UserInfo[F], jr: JoinRoom, rs: Vector[RoomInfo[F]]): (Vector[RoomInfo[F]], Option[RoomInfo[F]]) = {
    val updatedRoom = for {
      room <- rs.find(_.name == jr.name)
      users <- if (room.users.forall(_.u.id != u.u.id)) Some(room.users :+ u) else None
    } yield room.copy(users = users)

    updatedRoom.fold((rs, none[RoomInfo[F]])) { updatedR =>
      val roomIndex = rs.indexWhere(_.name == updatedR.name)
      (rs.updated(roomIndex, updatedR), Some(updatedR))
    }
  }

  private[this] def addMessage(m: Msg, roomName: String, rs: Vector[RoomInfo[F]]): (Vector[RoomInfo[F]], Option[RoomInfo[F]]) = {
    rs.find(_.name == roomName)
      .map(room => room.copy(msgs = room.msgs :+ m))
      .fold((rs, none[RoomInfo[F]])) { updatedRoom =>
        val roomIndex = rs.indexWhere(_.name == updatedRoom.name)
        (rs.updated(roomIndex, updatedRoom), Some(updatedRoom))
      }
  }
}

object RoomServiceImpl {
  def initRooms[F[_]]: Vector[RoomInfo[F]] = Vector(RoomInfo[F]("general"), RoomInfo[F]("funny"), RoomInfo[F]("serious"))

  def apply[F[_] : Effect](): F[RoomService[F]] = for {
    rooms <- Ref.of[F, Vector[RoomInfo[F]]](initRooms)
    roomService = new RoomServiceImpl[F](rooms)
  } yield roomService
}


