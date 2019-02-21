package pl.oen.logorrhea2.services

import cats.implicits._
import cats.effect.Effect
import cats.effect.concurrent.Ref
import pl.oen.logorrhea2.services.RoomService.RoomInfo
import pl.oen.logorrhea2.shared.Room

class RoomServiceImpl[F[_] : Effect](rooms: Ref[F, Vector[RoomInfo[F]]]) extends RoomService[F] {
  override def createRoom(name: String): F[Option[RoomInfo[F]]] = for {
    result <- rooms.modify(ri => addRoom(ri, name))
  } yield result

  override def getRooms(): F[Vector[Room]] = for {
    rms <- rooms.get
    roomsNames = rms.map(roomToData)
  } yield roomsNames

  private[this] def addRoom(ris: Vector[RoomInfo[F]], name: String): (Vector[RoomInfo[F]], Option[RoomInfo[F]]) = {
    def newRoom: (Vector[RoomInfo[F]], Option[RoomInfo[F]]) = {
      val created = RoomInfo[F](name)
      (ris :+ created, Some(created))
    }

    ris.find(_.name == name)
      .fold(newRoom)(_ => (ris, None))
  }
}

object RoomServiceImpl {
  def initRooms[F[_]]: Vector[RoomInfo[F]] = Vector(RoomInfo[F]("general"), RoomInfo[F]("funny"), RoomInfo[F]("serious"))

  def apply[F[_] : Effect](): F[RoomService[F]] = for {
    rooms <- Ref.of[F, Vector[RoomInfo[F]]](initRooms)
    roomService = new RoomServiceImpl[F](rooms)
  } yield roomService
}


