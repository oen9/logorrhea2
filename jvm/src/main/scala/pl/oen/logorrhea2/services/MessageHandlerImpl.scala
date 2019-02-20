package pl.oen.logorrhea2.services

import cats.effect.Effect
import cats.implicits._
import pl.oen.logorrhea2.shared._

class MessageHandlerImpl[F[_] : Effect](userService: UserService[F]) extends MessageHandler[F] {
  override def handle(id: Long, d: Data): F[Data] = d match {
//    case ChangeName(newName) => userService.changeName(id, newName).map(_ => Success(d))
    case ChangeName(newName) => for {
      _ <- userService.changeName(id, newName)
      logMsg = LogStr(s"user $id changed name to $newName")
      users <- userService.getUsers
      _ <- users.foldLeft(Effect[F].unit)((res, u) => res *> u.topic.publish1(logMsg))
    } yield Success(d)

    case unknown => Effect[F].pure(Error(s"unknown message $unknown"))
  }
}

object MessageHandlerImpl {
  def apply[F[_] : Effect](userService: UserService[F]): MessageHandlerImpl[F] = new MessageHandlerImpl[F](userService)
}
