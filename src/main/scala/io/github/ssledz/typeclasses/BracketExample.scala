package io.github.ssledz.typeclasses

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{Executors, ThreadFactory, TimeUnit}

import cats.effect.{Bracket, ExitCase, ExitCode, IO, IOApp, Resource, SyncIO}
import cats.implicits._
import io.github.ssledz.typeclasses.BracketExample.IntResource.{FailingClose, FailingRead, SafeClose}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object BracketExample extends IOApp with IOApp.WithContext {

  //noinspection DuplicatedCode
  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO(println("=========== bracket ==========="))
      _ <- bracketExample(IntResource.const(1))
      //      _ <- bracketExample(dbg("creating failing resource") *> IO(new FailingRead with FailingClose)).attempt.flatMap(a => dbg(a.toString))
      _ <- bracketExample(dbg("creating failing read resource") *> IO(new FailingRead with SafeClose {})).attempt.flatMap(a => dbg(a.toString))
      _ <- bracketExample(dbg("creating failing close resource") *> IO(new FailingClose { def readInt: Int = 1 })).attempt.flatMap(a => dbg(a.toString))
      _ <- IO(println("=========== bracketCase ==========="))
      _ <- bracketCaseExample(IntResource.const(1))
//      _ <- bracketCaseExample(dbg("creating failing resource") *> IO(new FailingRead with FailingClose)).attempt.flatMap(a => dbg(a.toString))
      _ <- bracketCaseExample(dbg("creating failing read resource") *> IO(new FailingRead with SafeClose {})).attempt.flatMap(a => dbg(a.toString))
      _ <- bracketCaseExample(dbg("creating failing close resource") *> IO(new FailingClose { def readInt: Int = 1 })).attempt.flatMap(a => dbg(a.toString))
      _ <- IO(println("=========== uncancelable ==========="))
      _ <- uncancelableExample
      _ <- IO(println("=========== guarantee ==========="))
      _ <- guaranteeExample
      _ <- IO(println("=========== guaranteeCase ==========="))
      _ <- guaranteeCaseExample.attempt.flatMap(a => dbg(a.toString))
      _ <- IO(println("=========== onCancel ==========="))
      _ <- onCancelExample
      _ <- dbg("Finished")
    } yield ExitCode.Success

  def unsafeDbg(msg: String): Unit = println(s"[${Thread.currentThread.getName}] $msg")

  def dbg(msg: String): IO[Unit] = IO(unsafeDbg(msg))

  sealed trait IntResource extends AutoCloseable {
    def readInt: Int
  }

  // F.guarantee(fa)(f) <-> F.bracket(F.unit)(_ => fa)(_ => f)
  def guaranteeExample: IO[Unit] = {
    val task = for {
      res <- IntResource.const(1)
      _ <- dbg("read int: " + res.readInt)
    } yield ()

    Bracket[IO, Throwable].guarantee(task)(dbg("running finalizer"))
  }

  // F.guaranteeCase(fa)(f) <-> F.bracketCase(F.unit)(_ => fa)((_, e) => f(e))
  def guaranteeCaseExample: IO[Unit] = {
    val task = for {
      res <- IntResource.const(1)
      _ <- dbg("read int: " + res.readInt)
      _ <- IO.raiseError(new RuntimeException("error during reading..."))
    } yield ()

    Bracket[IO, Throwable].guaranteeCase(task) {
      case ExitCase.Completed => dbg("running finalizer in case of completed")
      case ExitCase.Error(e) => dbg("running finalizer in case of error: " + e)
      case ExitCase.Canceled => dbg("running finalizer in case of canceled")
    }
  }

  def onCancelExample: IO[Unit] = {
    val task: IO[Int] =
      for {
        f1 <- countTo(10).start
        f2 <- (dbg("getting value") *> f1.join).start
        _ <- dbg("sleeping") *> IO.sleep(500.millis)
        _ <- f1.cancel
//        c <- f2.join
        c <- IO.pure(-11)
      } yield c
    Bracket[IO, Throwable].onCancel(task)(dbg("restoring counter")).flatMap(c => dbg(s"counter: $c"))
  }

  def uncancelableExample: IO[Unit] =
    for {
      c1 <- countTo(2)
      _ <- dbg(s"counted to c1 = $c1")
      f2 <- countTo(10).start
      _ <- dbg("sleeping") *> IO.sleep(500.millis)
      _ <- f2.cancel
      _ <- dbg("lets't try uncancelable")
//      c2 <- f2.join
//      _ <- dbg(s"counted to c2 = $c2")
      f3 <- Bracket[IO, Throwable].uncancelable(countTo(10)).start
      f4 <- (dbg("getting c3") *> f3.join).start
      _ <- dbg("sleeping") *> IO.sleep(100.millis)
      _ <- f3.cancel
      c3 <- f4.join
      _ <- dbg(s"counted to c3 = $c3")
    } yield ()

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
          unsafeDbg(s"cb(Right($i))")
          cb(Right(i))
        }
      }
      scheduler.execute(r)
      IO(go.set(false))
    }

  protected def executionContextResource: Resource[SyncIO, ExecutionContext] =
    Resource
      .make(SyncIO(Executors.newFixedThreadPool(8, threadFactory("io-app-thread"))))(pool =>
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
