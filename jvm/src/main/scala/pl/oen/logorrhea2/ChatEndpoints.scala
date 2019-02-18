package pl.oen.logorrhea2

import cats.effect.{ConcurrentEffect, Effect, Timer}
import cats.implicits._
import fs2.concurrent.Queue
import fs2.{Pipe, Stream}
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import pl.oen.logorrhea2.services.{MessageHandler, UserService}
import pl.oen.logorrhea2.shared.{Data, LogStr, UnknownData, User}

import scala.concurrent.duration.DurationDouble

class ChatEndpoints[F[_] : ConcurrentEffect : Timer](userService: UserService[F],
                                                     messageHandler: MessageHandler[F]) extends Http4sDsl[F] {
  def endpoints(): HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "chat" =>

      def echoReply(id: Long): Pipe[F, WebSocketFrame, WebSocketFrame] = _
        .collect { case Text(msg, _) => decode[Data](msg).fold(_ => UnknownData(msg), identity) }
        .evalMap(d => messageHandler.handle(id, d))
        .map(toWebsockData)

      def createWebSock(u: User, q: Queue[F, WebSocketFrame]) = {
        val id = Stream(u).map(toWebsockData)
        val ping = Stream.awakeEvery[F](100.seconds).map(p => toWebsockData(s"ping msg $p"))

        val d = q.dequeue.through(echoReply(u.id))
        val e = q.enqueue

        val onClose = for {
          _ <- userService.removeUser(u.id)
          users <- userService.getUsers
          _ <- Effect[F].delay(users.foreach(println))
        } yield ()

        WebSocketBuilder[F].build(id ++ d.merge(ping), e, onClose = onClose)
      }

      for {
        user <- userService.genNewUser()
        queue <- Queue.unbounded[F, WebSocketFrame]
        ws <- createWebSock(user, queue)
      } yield ws
  }

  def toWebsockData(data: Data): Text = Text(data.asJson.noSpaces)

  def toWebsockData(s: String): Text = toWebsockData(LogStr(s))
}

object ChatEndpoints {
  def apply[F[_] : ConcurrentEffect : Timer](userService: UserService[F], messageHandler: MessageHandler[F]): ChatEndpoints[F] =
    new ChatEndpoints[F](userService, messageHandler)
}
