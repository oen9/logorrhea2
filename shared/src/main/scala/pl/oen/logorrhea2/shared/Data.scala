package pl.oen.logorrhea2.shared

sealed trait Data
case class LogStr(log: String) extends Data
case class User(id: Long, name: String = "unknown") extends Data
