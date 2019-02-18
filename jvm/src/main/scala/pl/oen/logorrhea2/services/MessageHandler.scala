package pl.oen.logorrhea2.services

import cats.effect.Effect
import cats.implicits._
import pl.oen.logorrhea2.shared.{ChangeName, Data, Error, Success}

class MessageHandler[F[_]: Effect](userService: UserService[F]) {
  def handle(id: Long, d: Data): F[Data] = d match {
    case ChangeName(newName) => userService.changeName(id, newName).map(_ => Success(d))
    case unknown => Effect[F].pure(Error(s"unknown message $unknown"))
  }
}

object MessageHandler {
  def apply[F[_] : Effect](userService: UserService[F]): MessageHandler[F] = new MessageHandler[F](userService)
}
