package pl.oen.logorrhea2.services

import cats.effect.Effect
import cats.effect.concurrent.Ref
import cats.implicits._
import pl.oen.logorrhea2.shared.User

class UserService[F[_] : Effect](idCounterState: Ref[F, Long],
                                 users: Ref[F, Vector[User]]) {

  def genNewUser(): F[User] = for {
    id <- nextId()
    user = User(id)
    _ <- addUserToList(user)
  } yield user

  def removeUser(id: Long) = users.update(_.filter(_.id != id))

  def getUsers = users.get

  def changeName(id: Long, newName: String): F[Unit] = users.update(_.map{ u =>
    if (id == u.id) u.copy(name = newName) else u
  })

  private[this] def addUserToList(u: User) = users.update(_ :+ u)
  private[this] def nextId(): F[Long] = idCounterState.modify(curr => (curr+1, curr))
}

object UserService {
  def apply[F[_] : Effect](): F[UserService[F]] = for {
    idCounterState <- Ref.of[F, Long](1)
    users <- Ref.of[F, Vector[User]](Vector.empty)
    userIdProvider = new UserService[F](idCounterState, users)
  } yield userIdProvider
}
