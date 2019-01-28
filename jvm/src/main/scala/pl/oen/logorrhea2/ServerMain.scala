package pl.oen.logorrhea2

import java.util.concurrent.Executors

import cats.effect._
import cats.implicits._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import pl.oen.logorrhea2.config.AppConfig

import scala.concurrent.ExecutionContext

object ServerMain extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    createServer[IO]()
  }

  def createServer[F[_] : ContextShift : ConcurrentEffect : Timer](): F[ExitCode] = {
    for {
      conf <- AppConfig.read()
      blockingEc = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
      staticEndpoints = StaticEndpoints[F](blockingEc)
      exitCode <- BlazeServerBuilder[F]
        .bindHttp(conf.http.port, conf.http.host)
        .withHttpApp(staticEndpoints.endpoints().orNotFound)
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    } yield exitCode
  }
}
