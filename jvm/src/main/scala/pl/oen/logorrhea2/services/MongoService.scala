package pl.oen.logorrhea2.services

import cats.effect.Effect
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
  def addMsg(roomName: String, msg: Msg): F[Unit]
}

object MongoService {
  def apply[F[_] : Effect](mongoUri: String)(implicit dbEc: ExecutionContext): F[MongoService[F]] = {
    import pl.oen.logorrhea2.tclass.LiftAny._

    def connectToDb(): F[DefaultDB] = {
      val driver = MongoDriver()
      val parsedUri = MongoConnection.parseURI(mongoUri)
      val connection = parsedUri.map(driver.connection)
      val dbName = parsedUri.map(_.db.getOrElse("logrrhea2"))

      for {
        conn <- Effect[F].fromTry(connection)
        name <- Effect[F].fromTry(dbName)
        db <- conn.database(name).toF
      } yield db
    }

    for {
      db <- connectToDb()
      dbConfig = db.collection(StorageData.CONFIGS_COLLECTION_NAME): BSONCollection
      dbRooms = db.collection(StorageData.ROOMS_COLLECTION_NAME): BSONCollection
      mongoService = new MongoServiceImpl[F](dbConfig, dbRooms, dbEc)
    } yield mongoService
  }
}
