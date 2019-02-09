package pl.oen.logorrhea2.services

import cats.effect.Effect
import cats.effect.concurrent.Ref
import cats.implicits._
import pl.oen.logorrhea2.shared.User

class UserService[F[_] : Effect](stateRef: Ref[F, Long]) {

  def nextId(): F[Long] = stateRef.modify(curr => (curr+1, curr))

  def genNewUser(): F[User] = for {
    id <- nextId()
  } yield User(id)
}

object UserService {
  def apply[F[_] : Effect](): F[UserService[F]] = for {
    stateRef <- Ref.of[F, Long](1)
    userIdProvider = new UserService[F](stateRef)
  } yield userIdProvider
}
