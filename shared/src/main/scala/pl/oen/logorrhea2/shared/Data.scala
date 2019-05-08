package pl.oen.logorrhea2.shared

import monocle.macros.Lenses

sealed trait Data
case class LogStr(log: String) extends Data
case class UnknownData(json: String) extends Data

sealed trait Result extends Data
case class Success(data: Data) extends Result
case class Error(msg: String) extends Result

@Lenses case class User(id: Long, name: String = "unknown") extends Data
case class Msg(user: User, msg: String) extends Data
case class Room(name: String, users: Vector[User], msgs: Vector[Msg]) extends Data
case class Rooms(rooms: Vector[Room]) extends Data
case class RoomsNames(names: Vector[String]) extends Data

sealed trait Cmd extends Data
case class ChangeName(newName: String) extends Cmd
case class AddRoom(name: String) extends Cmd
case class JoinRoom(name: String) extends Cmd
case object AbandonRoom extends Cmd
case class RegisterMessage(msg: Msg, roomName: String) extends Cmd
case class ChangeRoomName(newRoomName: String, oldRoomName: String) extends Cmd
case class RemoveRoom(roomName: String) extends Cmd

sealed trait Evt extends Data
case class RoomAdded(name: String) extends Evt
case class JoinedRoom(room: Room) extends Evt
case class SomeoneJoinedRoom(user: User) extends Evt
case class SomeoneAbandonedRoom(user: User) extends Evt
case object RoomAbandoned extends Evt
case class MessageRegistered(msg: Msg) extends Evt
case class RoomNameChanged(newRoomName: String, oldRoomName: String) extends Evt
case class RoomRemoved(roomName: String) extends Evt
