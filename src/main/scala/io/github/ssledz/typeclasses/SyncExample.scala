package io.github.ssledz.typeclasses

import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import io.github.ssledz.utils.Logger

object SyncExample extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- suspendExample
    } yield ExitCode.Success

  def suspendExample: IO[Unit] = {
    val m1 = Sync[IO].pure(Logger.unsafeDbg("message"))
    val m2 = Sync[IO].suspend(Sync[IO].pure(Logger.unsafeDbg("suspended message 2")))
    val m3 = Sync[IO].suspend(Sync[IO].pure(Logger.unsafeDbg("suspended message 3")))
    m2 *> m1 *> m3
  }

}
