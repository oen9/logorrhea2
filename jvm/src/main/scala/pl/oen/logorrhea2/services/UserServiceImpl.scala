package pl.oen.logorrhea2.services

import cats.effect.ConcurrentEffect
import cats.effect.concurrent.Ref
import cats.implicits._
import fs2.concurrent.Topic
import pl.oen.logorrhea2.services.UserService.UserInfo
import pl.oen.logorrhea2.shared.{Data, LogStr, User}

class UserServiceImpl[F[_] : ConcurrentEffect](idCounterState: Ref[F, Long],
                                     users: Ref[F, Vector[UserInfo[F]]])
  extends UserService[F] {

  override def genNewUser(): F[UserInfo[F]] = for {
    id <- nextId()
    topic <- Topic[F, Data](LogStr(""))
    userInfo = UserInfo[F](User(id), topic)
    _ <- addUserToList(userInfo)
  } yield userInfo

  override def removeUser(id: Long): F[Unit] = users.update(_.filter(_.u.id != id))

  override def getUsers: F[Vector[UserInfo[F]]] = users.get

  override def getUser(id: Long): F[Option[UserInfo[F]]] = for {
    uiList <- users.get
    ui = uiList.find(_.u.id == id)
  } yield ui

  override def changeName(id: Long, newName: String): F[Unit] = users.update(_.map { ui =>
    val setUserName = (UserInfo.u[F] composeLens User.name).set(newName)
    if (id == ui.u.id) setUserName(ui) else ui
  })

  private[this] def addUserToList(u: UserInfo[F]) = users.update(_ :+ u)
  private[this] def nextId(): F[Long] = idCounterState.modify(curr => (curr + 1, curr))
}


object UserServiceImpl {
  def apply[F[_] : ConcurrentEffect](): F[UserService[F]] = for {
    idCounterState <- Ref.of[F, Long](1)
    users <- Ref.of[F, Vector[UserInfo[F]]](Vector.empty)
    userIdProvider = new UserServiceImpl[F](idCounterState, users)
  } yield userIdProvider

}