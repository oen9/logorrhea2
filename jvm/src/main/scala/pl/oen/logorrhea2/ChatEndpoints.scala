package pl.oen.logorrhea2

import cats.Applicative
import cats.effect.{ConcurrentEffect, Effect, Timer}
import cats.implicits._
import fs2.{Pipe, Stream}
import fs2.concurrent.Queue
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.{Close, Text}
import pl.oen.logorrhea2.services.UserService
import pl.oen.logorrhea2.shared.{Data, LogStr, User}

import scala.concurrent.duration.DurationDouble

class ChatEndpoints[F[_] : ConcurrentEffect : Timer](uip: UserService[F]) extends Http4sDsl[F] {
  def endpoints(): HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "chat" =>
      val echoReply: Pipe[F, WebSocketFrame, WebSocketFrame] =
        _.collect {
          case Text(msg, _) => toWebsockData("You sent the server: " + msg)
          case Close(_) => toWebsockData("Stream closed!")
          case unknown => toWebsockData(s"Something new $unknown")
        }.evalMap(t => Effect[F].delay {
          println(t)
          t
        })

      def createWebSock(u: User, q: Queue[F, WebSocketFrame]) = {
        val ping = Stream.awakeEvery[F](10.seconds).map(p => toWebsockData(s"ping msg $p"))
        val id = Stream(u).map(toWebsockData)
        val d = q.dequeue.through(echoReply)
        val e = q.enqueue
        WebSocketBuilder[F].build(id ++ d.merge(ping), e, onClose = Effect[F].delay(println(s"onClose $u")))
      }

      for {
        _ <- Applicative[F].unit
        user <- uip.genNewUser()
        ws <- Queue
          .unbounded[F, WebSocketFrame]
          .flatMap(createWebSock(user, _))
      } yield ws
  }

  def toWebsockData(data: Data): Text = Text(data.asJson.noSpaces)

  def toWebsockData(s: String): Text = toWebsockData(LogStr(s))
}

object ChatEndpoints {
  def apply[F[_] : ConcurrentEffect : Timer](uip: UserService[F]): ChatEndpoints[F] = new ChatEndpoints[F](uip)
}