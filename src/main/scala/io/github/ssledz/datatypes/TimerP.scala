package io.github.ssledz.datatypes

import java.util.concurrent.{ScheduledExecutorService, ScheduledThreadPoolExecutor, TimeUnit}

import cats.effect.{ExitCode, IO, IOApp, Timer}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import TimerPInstances._

object TimerP extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    implicit val g = scala.concurrent.ExecutionContext.Implicits.global
    implicit val sc: ScheduledExecutorService = new ScheduledThreadPoolExecutor(1)

    for {
      _ <- a
      _ <- b
    } yield ExitCode.Success
  }
}

object TimerPInstances {

  def a(implicit t: Timer[IO]): IO[Unit] =
    for {
      t1 <- Timer[IO].clock.realTime(TimeUnit.SECONDS)
      _ <- IO(println(s"seconds $t1"))
      _ <- Timer[IO].sleep(1.seconds)
      t2 <- Timer[IO].clock.realTime(TimeUnit.SECONDS)
      _ <- IO(println(s"seconds $t2"))
    } yield ()

  def b(implicit g: ExecutionContext, sc: ScheduledExecutorService): IO[Unit] =
    for {
      _ <- IO.timer(g, sc).sleep(1.seconds)
      _ = sc.shutdown()
    } yield ()

}
