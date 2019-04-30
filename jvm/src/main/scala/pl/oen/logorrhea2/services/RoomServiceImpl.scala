package pl.oen.logorrhea2.services

import cats.effect.Effect
import cats.effect.concurrent.Ref
import cats.implicits._
import pl.oen.logorrhea2.services.RoomService.RoomInfo
import pl.oen.logorrhea2.services.UserService.UserInfo
import pl.oen.logorrhea2.shared.{JoinRoom, Msg, Room}

class RoomServiceImpl[F[_] : Effect](rooms: Ref[F, Vector[RoomInfo[F]]],
                                     mongoService: MongoService[F]) extends RoomService[F] {

  override def createRoom(name: String): F[Option[RoomInfo[F]]] = for {
    result <- rooms.modify(ri => addRoom(ri, name))
    _ <- result.fold(Effect[F].unit)(mongoService.createRoom)
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

  override def registerMessage(msg: Msg, roomName: String): F[Option[RoomInfo[F]]] = for {
    room <- rooms.modify(addMessage(msg, roomName, _))
    _ <- room.fold(Effect[F].unit)(r => mongoService.addMsg(r.name, msg))
  } yield room

  override def changeRoomName(newRoomName: String, oldRoomName: String): F[Option[RoomInfo[F]]] = for {
    room <- rooms.modify(changeName(newRoomName, oldRoomName, _))
    _ <- room.fold(Effect[F].unit)(_ => mongoService.changeRoomName(newRoomName, oldRoomName))
  } yield room

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

  private[this] def addMessage(m: Msg, roomName: String, rs: Vector[RoomInfo[F]]): (Vector[RoomInfo[F]], Option[RoomInfo[F]]) =
    modifyRoomByName(roomName, rs) { room =>
      room.copy(msgs = room.msgs :+ m)
    }

  private[this] def changeName(newRoomName: String, oldRoomName: String, rs: Vector[RoomInfo[F]]): (Vector[RoomInfo[F]], Option[RoomInfo[F]]) =
    modifyRoomByName(oldRoomName, rs) { room =>
      room.copy(name = newRoomName, users = Vector())
    }

  private[this] def modifyRoomByName(roomName: String, rs: Vector[RoomInfo[F]])(op: RoomInfo[F] => RoomInfo[F]): (Vector[RoomInfo[F]], Option[RoomInfo[F]]) =
    rs.find(_.name == roomName)
      .map(op)
      .fold((rs, none[RoomInfo[F]])) { updatedRoom =>
        val roomIndex = rs.indexWhere(_.name == roomName)
        (rs.updated(roomIndex, updatedRoom), Some(updatedRoom))
      }
}

object RoomServiceImpl {
  def apply[F[_] : Effect](mongoService: MongoService[F]): F[RoomService[F]] = for {
    initRooms <- mongoService.getRooms()
    rooms <- Ref.of[F, Vector[RoomInfo[F]]](initRooms)
    roomService = new RoomServiceImpl[F](rooms, mongoService)
  } yield roomService
}
