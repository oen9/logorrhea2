package pl.oen.logorrhea2.services

import cats.effect.Effect
import pl.oen.logorrhea2.shared.Data

trait MessageHandler[F[_]] {
  def handle(id: Long, d: Data): F[Data]
}

object MessageHandler {
  def apply[F[_] : Effect](userService: UserService[F],
                           roomService: RoomService[F]): MessageHandler[F] =
    MessageHandlerImpl[F](userService, roomService)
}
