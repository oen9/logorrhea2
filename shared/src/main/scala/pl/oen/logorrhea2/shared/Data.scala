package pl.oen.logorrhea2.shared

import monocle.macros.Lenses

sealed trait Data
case class LogStr(log: String) extends Data
case class UnknownData(json: String) extends Data
case class Success(data: Data) extends Data
case class Error(msg: String) extends Data

@Lenses case class User(id: Long, name: String = "unknown") extends Data
case class Msg(userId: Long, userName: String, msg: String) extends Data
case class Room(name: String, users: Vector[User], msgs: Vector[Msg]) extends Data
case class Rooms(rooms: Vector[Room]) extends Data
case class RoomsNames(names: Vector[String]) extends Data

sealed trait Cmd extends Data
case class ChangeName(newName: String) extends Cmd
case class AddRoom(name: String) extends Cmd

sealed trait Evt extends Data
case class RoomAdded(name: String) extends Evt
