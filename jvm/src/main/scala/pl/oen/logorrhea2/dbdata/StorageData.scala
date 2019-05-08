package pl.oen.logorrhea2.dbdata

import pl.oen.logorrhea2.shared.{Msg, User}
import reactivemongo.bson.{BSONDocumentReader, BSONDocumentWriter, Macros}

object StorageData {
  val CONFIGS_COLLECTION_NAME = "configs"
  val ROOMS_COLLECTION_NAME = "rooms"
  val ROOMS_REMOVED_COLLECTION_NAME = "roomsRemoved"

  case class ConfigState(userCounter: Int)
  case class RoomState(name: String, msgs: Vector[Msg] = Vector())

  implicit def configStateWriter: BSONDocumentWriter[ConfigState] = Macros.writer[ConfigState]
  implicit def configStateReader: BSONDocumentReader[ConfigState] = Macros.reader[ConfigState]

  implicit def roomStateWriter: BSONDocumentWriter[RoomState] = Macros.writer[RoomState]
  implicit def roomStateReader: BSONDocumentReader[RoomState] = Macros.reader[RoomState]
  implicit def msgWriter: BSONDocumentWriter[Msg] = Macros.writer[Msg]
  implicit def msgReader: BSONDocumentReader[Msg] = Macros.reader[Msg]
  implicit def userWriter: BSONDocumentWriter[User] = Macros.writer[User]
  implicit def userReader: BSONDocumentReader[User] = Macros.reader[User]
}
