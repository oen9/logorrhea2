package pl.oen.logorrhea2.services

import cats.effect.Effect
import cats.implicits._
import pl.oen.logorrhea2.shared._

class MessageHandlerImpl[F[_] : Effect](userService: UserService[F],
                                        roomService: RoomService[F]) extends MessageHandler[F] {

  override def handle(id: Long, d: Data): F[Data] = d match {
    case ChangeName(newName) => for {
      _ <- userService.changeName(id, newName)
      logMsg = LogStr(s"user $id changed name to $newName")
      _ <- userService.publish(logMsg)
    } yield Success(d)

    case AddRoom(name) => for {
      created <- roomService.createRoom(name)
      roomAddedEvt = created.map(c => RoomAdded(c.name))
      _ <- roomAddedEvt.fold(Effect[F].unit)(d => userService.publish(d))
      result = roomAddedEvt.fold(Error(s"can't create room $name") : Data)(_ => Success(d))
    } yield result

    case unknown => Effect[F].pure(Error(s"unknown message $unknown"))
  }
}

object MessageHandlerImpl {
  def apply[F[_] : Effect](userService: UserService[F], roomService: RoomService[F]): MessageHandlerImpl[F] =
    new MessageHandlerImpl[F](userService, roomService)
}
