package io.github.ssledz.typeclasses

import cats.effect.{Bracket, ExitCode, IO, IOApp}
import cats.implicits._
import io.github.ssledz.typeclasses.BracketExample.IntResource.{FailingClose, FailingRead, SafeClose}

object BracketExample extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- bracketExample(IntResource.const(1))
      _ <- bracketExample(dbg("creating failing resource") *> IO(new FailingRead with FailingClose)).attempt.flatMap(a => dbg(a.toString))
      _ <- bracketExample(dbg("creating failing read resource") *> IO(new FailingRead with SafeClose {})).attempt.flatMap(a => dbg(a.toString))
      _ <- bracketExample(dbg("creating failing close resource") *> IO(new FailingClose { def readInt: Int = 1 })).attempt.flatMap(a => dbg(a.toString))
      _ <- dbg("Finished")
    } yield ExitCode.Success

  def unsafeDbg(msg: String): Unit = println(s"[${Thread.currentThread.getName}] $msg")

  def dbg(msg: String): IO[Unit] = IO(unsafeDbg(msg))

  sealed trait IntResource extends AutoCloseable {
    def readInt: Int
  }

  def bracketExample(resource: IO[IntResource]): IO[Unit] =
    Bracket[IO, Throwable].bracket(resource) { resource =>
      dbg("reading int from resource: " + resource.readInt)
    }(resource => IO(resource.close()))

  object IntResource {

    def const(value: Int): IO[IntResource] = dbg("creating int const resource") *> IO(ConstIntResource(value))

    trait SafeClose extends IntResource {
      def close(): Unit = unsafeDbg("Closing resource")
    }

    trait FailingRead extends IntResource {
      def readInt: Int = throw new RuntimeException("failing read")

    }

    trait FailingClose extends IntResource {
      def close(): Unit = throw new RuntimeException("failing close")
    }

    case class ConstIntResource(readInt: Int) extends IntResource {
      def close(): Unit = unsafeDbg("Closing const resource")
    }

  }

}
