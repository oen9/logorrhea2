package pl.oen.logorrhea2.services

import cats.effect.Effect
import cats.implicits._
import pl.oen.logorrhea2.dbdata.StorageData
import pl.oen.logorrhea2.dbdata.StorageData.{ConfigState, RoomState}
import pl.oen.logorrhea2.services.RoomService.RoomInfo
import pl.oen.logorrhea2.shared.Msg
import reactivemongo.api.Cursor
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext

class MongoServiceImpl[F[_] : Effect](dbConfig: BSONCollection, dbRooms: BSONCollection, dbRoomsRemoved: BSONCollection,
                                      implicit val dbEc: ExecutionContext) extends MongoService[F] {

  import pl.oen.logorrhea2.dbdata.StorageData.msgWriter
  import pl.oen.logorrhea2.tclass.LiftAny._

  def initRooms: Vector[RoomInfo[F]] = Vector(RoomInfo[F]("general"), RoomInfo[F]("funny"), RoomInfo[F]("serious"))

  override def getCfg(): F[StorageData.ConfigState] = for {
    oldCfg <- dbConfig.find(BSONDocument(), Option.empty).one[ConfigState].toF
    result <- oldCfg.fold({
      val newCfg = ConfigState(1)
      dbConfig.insert.one(newCfg).map(_ => newCfg).toF
    })(Effect[F].pure(_))
  } yield result

  override def incrementUserCounter(): F[Unit] = for {
    _ <- dbConfig.update.one(BSONDocument(), BSONDocument("$inc" -> BSONDocument("userCounter" -> 1))).toF
  } yield ()

  override def getRooms(): F[Vector[RoomInfo[F]]] = for {
    roomStates <- dbRooms.find(BSONDocument(), Option.empty).cursor[RoomState]().collect[Vector](-1, Cursor.FailOnError[Vector[RoomState]]()).toF
    maybeNotInitiatedRooms = if (roomStates.isEmpty) none[Vector[RoomState]] else Some(roomStates)
    initiatedRooms <- maybeNotInitiatedRooms.fold(
      initRooms.foldLeft(Effect[F].unit)((a, b) => a >> createRoom(b)) >> Effect[F].pure(initRooms)
    )(rss => Effect[F].pure(rss.map(roomStateToRoomInfo)))
  } yield initiatedRooms

  override def createRoom(roomInfo: RoomService.RoomInfo[F]): F[Unit] = for {
    _ <- dbRooms.insert.one(RoomState(roomInfo.name, roomInfo.msgs)).toF
  } yield ()

  override def changeRoomName(newRoomName: String, oldRoomName: String): F[Unit] = for {
    _ <- Effect[F].unit
    upd = BSONDocument("$set" -> BSONDocument("name" -> newRoomName))
    _ <- dbRooms.update.one(BSONDocument("name" -> oldRoomName), upd).toF
  } yield ()

  override def addMsg(roomName: String, msg: Msg): F[Unit] = for {
    _ <- Effect[F].unit
    upd = BSONDocument("$push" -> BSONDocument("msgs" -> msg))
    _ <- dbRooms.update.one(BSONDocument("name" -> roomName), upd).toF
  } yield ()

  override def removeRoom(roomInfo: RoomInfo[F]): F[Unit] = for {
    _ <- dbRoomsRemoved.insert.one(RoomState(roomInfo.name, roomInfo.msgs)).toF
    _ <- dbRooms.delete.one(BSONDocument("name" -> roomInfo.name)).toF
  } yield ()

  override def reviveRoom(roomName: String): F[Option[RoomInfo[F]]] = for {
    _ <- Effect[F].unit
    query = BSONDocument("name" -> roomName)
    revivedRoom <- dbRoomsRemoved.find(query, Option.empty).one[RoomState].toF
    _ <- revivedRoom.fold(Effect[F].unit)(_ => dbRoomsRemoved.delete.one(query).toF *> Effect[F].unit)
    roomInfo = revivedRoom.map(roomStateToRoomInfo)
  } yield roomInfo

  private[this] def roomStateToRoomInfo(rs: RoomState): RoomInfo[F] = RoomInfo[F](rs.name, msgs = rs.msgs)
}

object MongoServiceImpl {
  def apply[F[_] : Effect](dbConfig: BSONCollection,
                           dbRooms: BSONCollection,
                           dbRoomsRemoved: BSONCollection,
                           dbEc: ExecutionContext): MongoServiceImpl[F] =
    new MongoServiceImpl(dbConfig, dbRooms, dbRoomsRemoved, dbEc)
}
