package io.github.ssledz.typeclasses

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{Executors, ThreadFactory, TimeUnit}

import cats.effect.{Bracket, ExitCode, IO, IOApp, Resource, SyncIO}
import cats.implicits._
import io.github.ssledz.typeclasses.BracketExample.IntResource.{FailingClose, FailingRead, SafeClose}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object BracketExample extends IOApp with IOApp.WithContext {

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO(println("=========== bracket ==========="))
      _ <- bracketCaseExample(IntResource.const(1))
//      _ <- bracketCaseExample(dbg("creating failing resource") *> IO(new FailingRead with FailingClose)).attempt.flatMap(a => dbg(a.toString))
      _ <- bracketCaseExample(dbg("creating failing read resource") *> IO(new FailingRead with SafeClose {})).attempt.flatMap(a => dbg(a.toString))
      _ <- bracketCaseExample(dbg("creating failing close resource") *> IO(new FailingClose { def readInt: Int = 1 })).attempt.flatMap(a => dbg(a.toString))
      _ <- IO(println("=========== uncancelle ==========="))
      _ <- uncancelledExample
      _ <- dbg("Finished")
    } yield ExitCode.Success

  def unsafeDbg(msg: String): Unit = println(s"[${Thread.currentThread.getName}] $msg")

  def dbg(msg: String): IO[Unit] = IO(unsafeDbg(msg))

  sealed trait IntResource extends AutoCloseable {
    def readInt: Int
  }

  def uncancelledExample: IO[Unit] = {
    def countTo(max: Int): IO[Int] =
      IO.cancelable[Int] { cb =>
        val go = new AtomicBoolean(true)
        val r = new Runnable {
          def run(): Unit = {
            var i = 0
            while (go.get && i <= max) {
              unsafeDbg(s"counter: $i")
              Thread.sleep(100)
              i = i + 1
            }
            cb(Right(i))
          }
        }
        scheduler.execute(r)
        IO(go.set(false))
      }

    for {
      c1 <- countTo(2)
      _ <- dbg(s"counted to c1 = $c1")
      f2 <- countTo(10).start
      _ <- dbg("sleeping") *> IO.sleep(500.millis)
      _ <- f2.cancel
//      c2 <- f2.join
//      _ <- dbg(s"counted to c2 = $c2")
      f3 <- Bracket[IO, Throwable].uncancelable(countTo(10)).start
      _ <- f3.cancel
      c3 <- f3.join
      _ <- dbg(s"counted to c3 = $c3")
    } yield ()

  }

  def bracketCaseExample(resource: IO[IntResource]): IO[Unit] =
    Bracket[IO, Throwable].bracketCase(resource) { resource =>
      dbg("reading int from resource: " + resource.readInt)
    }((resource, exitCase) => dbg("exit case: " + exitCase) *> IO(resource.close()))

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

  protected def executionContextResource: Resource[SyncIO, ExecutionContext] =
    Resource
      .make(SyncIO(Executors.newFixedThreadPool(8, threadFactory("io-app-thread-"))))(pool =>
        SyncIO {
          pool.shutdown()
          pool.awaitTermination(10, TimeUnit.SECONDS)
      })
      .map(ExecutionContext.fromExecutorService)

  private def threadFactory(name: String): ThreadFactory = new ThreadFactory {
    def newThread(r: Runnable): Thread = {
      val th = new Thread(r)
      th.setName(s"$name-${th.getId}")
      th.setDaemon(true)
      th
    }
  }
}
