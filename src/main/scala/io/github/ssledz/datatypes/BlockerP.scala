package io.github.ssledz.datatypes

import java.util.concurrent.{ExecutorService, Executors}

import cats.effect._
import cats.implicits._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object BlockerP extends IOApp {

  def countDown: IO[Unit] = {
    val f: IO[Unit] = for {
      _ <- IO(println(1))
      _ <- Timer[IO].sleep(1.second)
    } yield ()
    f >> countDown
  }

  def readName[F[_] : Sync : ContextShift](blocker: Blocker): F[String] =
    blocker.delay {
      println("Enter your name: ")
      scala.io.StdIn.readLine()
    }

  def withoutIO: IO[Unit] = {
    val bec = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
    val blocker = Blocker.liftExecutionContext(bec)

    for {
      n <- readName[IO](blocker)
      _ <- IO(println(s"Hello, $n!"))
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] = {
    val name = Blocker[IO].use { blocker =>
      for {
        n <- readName[IO](blocker)
        _ <- IO(println(s"Hello, $n!"))
      } yield ()
    }

    for {
      _ <- IO.race(countDown, name)
    } yield ExitCode.Success
  }
}