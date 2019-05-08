package pl.oen.logorrhea2.services

import cats.effect.{Effect, Resource}
import cats.implicits._
import pl.oen.logorrhea2.dbdata.StorageData
import pl.oen.logorrhea2.dbdata.StorageData.ConfigState
import pl.oen.logorrhea2.services.RoomService.RoomInfo
import pl.oen.logorrhea2.shared.Msg
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.{DefaultDB, MongoConnection, MongoDriver}

import scala.concurrent.ExecutionContext

trait MongoService[F[_]] {
  def getCfg(): F[ConfigState]
  def incrementUserCounter(): F[Unit]
  def getRooms(): F[Vector[RoomInfo[F]]]
  def createRoom(roomInfo: RoomInfo[F]): F[Unit]
  def changeRoomName(newRoomName: String, oldRoomName: String): F[Unit]
  def addMsg(roomName: String, msg: Msg): F[Unit]
  def removeRoom(roomInfo: RoomInfo[F]): F[Unit]
  def reviveRoom(roomName: String): F[Option[RoomInfo[F]]]
}

object MongoService {
  def apply[F[_] : Effect](mongoUri: String)(implicit dbEc: ExecutionContext): Resource[F, MongoService[F]] = {
    import pl.oen.logorrhea2.tclass.LiftAny._

    def connectToDb(driver: MongoDriver): F[DefaultDB] = {
      val parsedUri = MongoConnection.parseURI(mongoUri)
      val connection = parsedUri.map(driver.connection)
      val dbName = parsedUri.map(_.db.getOrElse("logrrhea2"))

      for {
        conn <- Effect[F].fromTry(connection)
        name <- Effect[F].fromTry(dbName)
        db <- conn.database(name).toF
      } yield db
    }

    def createMongoService(db: DefaultDB) = {
      val dbConfig = db.collection(StorageData.CONFIGS_COLLECTION_NAME): BSONCollection
      val dbRooms = db.collection(StorageData.ROOMS_COLLECTION_NAME): BSONCollection
      val dbRoomsRemoved = db.collection(StorageData.ROOMS_REMOVED_COLLECTION_NAME): BSONCollection
      MongoServiceImpl[F](dbConfig, dbRooms, dbRoomsRemoved, dbEc)
    }

    for {
      driver <- Resource.make(Effect[F].delay(MongoDriver()))(driver => Effect[F].delay(driver.close()))
      db <- Resource.liftF(connectToDb(driver))
      mongoService = createMongoService(db)
    } yield mongoService
  }
}
