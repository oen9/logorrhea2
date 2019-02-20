package pl.oen.logorrhea2.shared

import monocle.macros.Lenses

sealed trait Data
case class LogStr(log: String) extends Data
@Lenses case class User(id: Long, name: String = "unknown") extends Data
case class UnknownData(json: String) extends Data
case class Success(data: Data) extends Data
case class Error(msg: String) extends Data

sealed trait Cmd extends Data
case class ChangeName(newName: String) extends Cmd
