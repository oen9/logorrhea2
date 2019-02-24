package pl.oen.logorrhea2.services

import cats.effect.Effect
import cats.implicits._
import pl.oen.logorrhea2.services.RoomService.RoomInfo
import pl.oen.logorrhea2.services.UserService.UserInfo
import pl.oen.logorrhea2.shared.{AbandonRoom, _}

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

    case jr@JoinRoom(name) => for {
      user <- userService.getUser(id)
      _ <- user.fold(Effect[F].unit)(abandonRoom)

      ri <- user.fold(Effect[F].pure(None: Option[RoomInfo[F]]))(u => roomService.joinRoom(u, jr))
      _ <- ri.fold(Effect[F].unit)(r => userService.joinRoom(id, Some(r.name)))

      published = (ri, user).bisequence.map { case (r, u) => publishJoinedRoom(r, u) }
      result <- published.fold(Effect[F].pure(Error(s"can't join the room $name") : Result))(_ *> Effect[F].pure(Success(jr)))
    } yield result

    case AbandonRoom => for {
      user <- userService.getUser(id)
      _ <- user.fold(Effect[F].unit)(abandonRoom)
    } yield Success(d)

    case unknown => Effect[F].pure(Error(s"unknown message $unknown"))
  }

  private[this] def publishJoinedRoom(r: RoomInfo[F], u: UserInfo[F]): F[Unit] = {
    val joinedRoom = JoinedRoom(RoomService.roomToData(r))
    val someoneJoinedRoom = SomeoneJoinedRoom(u.u)
    val usersToBroadcast = r.users.filter(_.u.id != u.u.id)

    for {
      _ <- userService.publish(joinedRoom, u)
      _ <- userService.publish(someoneJoinedRoom, usersToBroadcast)
    } yield ()
  }

  private[this] def abandonRoom(u: UserInfo[F]): F[Unit] = for {
    room <- roomService.abandonRoom(u)
    _ <- room.fold(Effect[F].unit)(r => userService.publish(SomeoneAbandonedRoom(u.u), r.users))
  } yield ()
}

object MessageHandlerImpl {
  def apply[F[_] : Effect](userService: UserService[F], roomService: RoomService[F]): MessageHandlerImpl[F] =
    new MessageHandlerImpl[F](userService, roomService)
}
