package pl.oen.logorrhea2.services

import cats.effect.ConcurrentEffect
import fs2.concurrent.Topic
import monocle.macros.Lenses
import pl.oen.logorrhea2.services.UserService.UserInfo
import pl.oen.logorrhea2.shared.{Data, User}

trait UserService[F[_]] {
  def genNewUser(): F[UserInfo[F]]
  def removeUser(id: Long): F[Unit]
  def getUsers: F[Vector[UserInfo[F]]]
  def getUser(id: Long): F[Option[UserInfo[F]]]
  def changeName(id: Long, newName: String): F[Unit]
  def joinRoom(id: Long, room: Option[String]): F[Unit]
  def changeRoomName(newRoomName: String, oldRoomName: String): F[Unit]
  def publish(data: Data): F[Unit]
  def publish(data: Data, receivers: Vector[UserInfo[F]]): F[Unit]

  def publish(data: Data, receiver: UserInfo[F]): F[Unit] = publish(data, Vector(receiver))
}

object UserService {
  def apply[F[_] : ConcurrentEffect](mongoService: MongoService[F]): F[UserService[F]] = UserServiceImpl(mongoService)

  @Lenses case class UserInfo[F[_]](u: User, topic: Topic[F, Data], room: Option[String] = None)
}
