package io.github.ssledz.typeclasses

import cats.Defer
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import io.github.ssledz.utils.Logger

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future
import scala.concurrent.duration._

object DeferExample extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- deferExample
    } yield ExitCode.Success

  def deferExample: IO[Unit] = {
    def future(msg: String): IO[Unit] = IO.fromFuture(IO.pure(Future(Logger.unsafeDbg(msg))))
    val f1 = future("processing in future")
    val f2 = Defer[IO].defer(future("processing in deferred future"))
    Logger.dbg("sleeping") *> IO.sleep(1000.millis) *> Logger.dbg("before") *> f2 *> f1
  }

}
