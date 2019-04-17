package pl.oen.logorrhea2

import java.util.concurrent.Executors

import cats.effect._
import cats.implicits._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import pl.oen.logorrhea2.config.AppConfig
import pl.oen.logorrhea2.services.{MessageHandler, MongoService, RoomService, UserService}

import scala.concurrent.ExecutionContext

object App extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    (for {
      blockingEc <- createEc[IO](4)
      dbEc <- createEc[IO](4)
    } yield (blockingEc, dbEc)).use { case (blockingEc, dbEc) =>
      createServer[IO](blockingEc, dbEc)
    }
  }

  def createServer[F[_] : ContextShift : ConcurrentEffect : Timer](blockingEc: ExecutionContext,
                                                                   dbEc: ExecutionContext): F[ExitCode] = {
    for {
      conf <- AppConfig.read()
      staticEndpoints = StaticEndpoints[F](blockingEc)
      mongoService <- MongoService[F](conf.mongo.uri)(Effect[F], dbEc)
      userService <- UserService[F](mongoService)
      roomService <- RoomService[F](mongoService)
      messageHandler = MessageHandler(userService, roomService)
      chatEndpoints = ChatEndpoints[F](userService, roomService, messageHandler)
      httpApp = (staticEndpoints.endpoints() <+> chatEndpoints.endpoints()).orNotFound
      exitCode <- BlazeServerBuilder[F]
        .bindHttp(conf.http.port, conf.http.host)
        .withHttpApp(httpApp)
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    } yield exitCode
  }

  def createEc[F[_] : Effect](nThreads: Int): Resource[F, ExecutionContext] = Resource[F, ExecutionContext](Effect[F].delay {
    val executor = Executors.newFixedThreadPool(nThreads)
    val ec = ExecutionContext.fromExecutor(executor)
    (ec, Effect[F].delay(executor.shutdown()))
  })
}
