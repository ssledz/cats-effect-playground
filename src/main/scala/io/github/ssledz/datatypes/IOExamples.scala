package io.github.ssledz.datatypes

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{Executors, ScheduledExecutorService}

import cats.effect.{ContextShift, ExitCode, IO, IOApp, Timer}
import cats.implicits._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object IOExamples extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
//    StackSafety.app.as(ExitCode.Success)
//    SyncEffects.app.as(ExitCode.Success)
//    AsyncEffects.app.as(ExitCode.Success)
    CancelableProcesses.app.as(ExitCode.Success)

}

object CancelableProcesses {

  def slowIteration(n: Int, label: String, duration: FiniteDuration = 500.millis)(implicit cs: ContextShift[IO], timer: Timer[IO]): IO[Unit] = IO.suspend {
    if (n <= 0) {
      putStrLn(s"$label: $n and finished")
    } else putStrLn(s"$label: $n") >> timer.sleep(duration) >> slowIteration(n - 1, label)
  }

  // Cancellation status is only checked after asynchronous boundaries (in the previous example timer.sleep played the role of async boundary)
  def notCancelableSlowIteration(n: Int, label: String)(implicit cs: ContextShift[IO]): IO[Unit] = IO.suspend {
    if (n <= 0) {
      putStrLn(s"$label: $n and finished")
    } else putStrLn(s"$label: $n") >> notCancelableSlowIteration(n - 1, label)
  }

  // cancelable
  def slowIteration2(n: Int, label: String, duration: FiniteDuration = 500.millis)(implicit ec: ExecutionContext): IO[Unit] = {
    val go = new AtomicBoolean(true)
    def inpureIter(n: Int): Unit = {
      def print(m: String) = println(s"[$currentThread] $m")
      var iter = n
      while (iter > 0 && go.get()) {
        print(s"$label: $iter")
        iter = iter - 1
        Thread.sleep(duration.toMillis)
      }
      print(s"$label: $iter and finished")
    }
    IO.cancelable { cb =>
      ec.execute(() => cb(Right(inpureIter(n))))
      IO(go.set(false)).void
    }

  }

  def currentThread: String = Thread.currentThread().getName

  def putStrLn(m: String): IO[Unit] = IO(println(s"[$currentThread] $m"))

  def app(implicit cs: ContextShift[IO], timer: Timer[IO]): IO[Unit] = {
    import concurrent.ExecutionContext.Implicits.global
    for {
      _ <- slowIteration(3, "it from 3")
      _ <- IO.race(slowIteration(3, "it from 3"), slowIteration(5, "it from 5"))
      _ <- IO.race(notCancelableSlowIteration(3, "not cancelable it from 3"), notCancelableSlowIteration(5, "not cancelable it from 5"))
      _ <- IO.race(slowIteration2(3, "it2 from 3"), slowIteration2(5, "it2 from 5"))
    } yield ()
  }

}

object AsyncEffects {

  import concurrent.ExecutionContext.Implicits.global

  val future1 = convert(Future(println("future 1")))
  val future2 = convert(Future(println("future 2")))

  def app(implicit cs: ContextShift[IO]): IO[Unit] = {
    implicit val se = Executors.newScheduledThreadPool(2)
    val future3 = IO.fromFuture(IO(Future(println("future 3"))))
    val future4 = IO.fromFuture(IO.pure(Future(println("future 4")))) // eager evaluation
    for {
      fiber <- (sleep(1000.millis) *> future1).start
      _ <- future1
      _ <- future2
      _ <- future3
      _ <- future4
      _ <- fiber.join
      _ <- IO(se.shutdown()).void
    } yield ()
  }

  def convert[A](fa: => Future[A]): IO[A] =
    IO.async { cb =>
      // This triggers evaluation of the by-name param and of onComplete,
      // so it's OK to have side effects in this callback
      fa.onComplete {
        case Success(a) => cb(Right(a))
        case Failure(e) => cb(Left(e))
      }
    }
  def sleep(d: FiniteDuration)(implicit sc: ScheduledExecutorService): IO[Unit] =
    IO.cancelable { cb =>
      val r = new Runnable { def run() = cb(Right(())) }
      val f = sc.schedule(r, d.length, d.unit)

      // Returning the cancellation token needed to cancel
      // the scheduling and release resources early
      IO(f.cancel(false)).void
    }

}

object SyncEffects {

  def putStrLn(value: String): IO[Unit] = IO(println(value))

  def app(implicit cs: ContextShift[IO]): IO[Unit] = {

    val helloWorld = IO(println("Hello World"))

    val thisIsWrong = IO.pure(println("THIS IS WRONG!"))

    for {
      _ <- helloWorld
      _ <- thisIsWrong
      _ <- IO.pure(25).flatMap(n => putStrLn(s"Number is: $n"))
      neverTask <- (IO.never *> putStrLn("This will be never called !")).start
      _ <- IO.unit
      _ <- IO.race(IO(Thread.sleep(1000)) *> putStrLn("Time out !!!"), neverTask.join)
    } yield ()
  }

}

object StackSafety {

  val app = for {
    fib4 <- StackSafety.fib(4)
    fib5 <- StackSafety.fib2(5)
    _ <- IO(println(s"fib4: $fib4"))
    _ <- IO(println(s"fib5: $fib5"))
  } yield ()

  def fib(n: Int, a: Long = 0, b: Long = 1): IO[Long] =
    IO(a + b).flatMap { b2 =>
      if (n > 0)
        fib(n - 1, b, b2)
      else
        IO.pure(a)
    }

  def fib2(n: Int, a: Long = 0, b: Long = 1): IO[Long] =
    IO.suspend {
      if (n > 0)
        fib(n - 1, b, a + b)
      else
        IO.pure(a)
    }
}
