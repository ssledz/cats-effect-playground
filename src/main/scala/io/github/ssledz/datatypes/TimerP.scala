package io.github.ssledz.datatypes

import java.util.concurrent.{ScheduledExecutorService, ScheduledThreadPoolExecutor, TimeUnit}

import cats.effect.{ExitCode, IO, IOApp, Timer}

import scala.concurrent.duration._

object TimerP extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val g = scala.concurrent.ExecutionContext.Implicits.global
    val sc: ScheduledExecutorService = new ScheduledThreadPoolExecutor(1)

    val a = for {
      t1 <- Timer[IO].clock.realTime(TimeUnit.SECONDS)
      _ <- IO(println(s"seconds $t1"))
      _ <- Timer[IO].sleep(1.seconds)
      t2 <- Timer[IO].clock.realTime(TimeUnit.SECONDS)
      _ <- IO(println(s"seconds $t2"))
    } yield ()

    val b = for {
      _ <- IO.timer(g, sc).sleep(1.seconds)
      _ = sc.shutdown()
    } yield ()

    for {
      _ <- a
      _ <- b
    } yield ExitCode.Success
  }
}
