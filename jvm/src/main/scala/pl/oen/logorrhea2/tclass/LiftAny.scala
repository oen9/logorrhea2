package pl.oen.logorrhea2.tclass

import cats.effect.{IO, LiftIO}

import scala.concurrent.Future

trait LiftAny[F[_], A[_], B] {
  def liftAny(a: A[B]): F[B]
}

object LiftAny {
  def liftAny[F[_], A[_], B](a: A[B])(implicit la: LiftAny[F, A, B]): F[B] = la.liftAny(a)

  implicit class liftAnyOps[A[_], B](a: A[B]) {
    def toF[F[_]](implicit la: LiftAny[F, A, B]): F[B] = la.liftAny(a)
  }

  implicit def futureLiftIO[F[_]: LiftIO, B]: LiftAny[F, Future, B] = (a: Future[B]) => IO.fromFuture(IO(a)).to[F]
}
