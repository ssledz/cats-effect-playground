package io.github.ssledz.datatypes

import cats.effect._
import cats.implicits._
import scala.concurrent.duration.MILLISECONDS

object ClockP extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    println(catchTimeNs().size)
    println(catchTimeMs().size)
    runMeasurements
  }

  private def catchTimeNs(): Iterable[Boolean] =
    (0 until 1000)
      .map(_ => System.nanoTime() == System.nanoTime())
      .filter(identity)

  private def catchTimeMs(): Iterable[Boolean] =
    (0 until 1000)
      .map(_ => System.currentTimeMillis() == System.currentTimeMillis())
      .filter(identity)

  private def runMeasurements: IO[ExitCode] =
    for {
      (f1, t1) <- measure(StackSafety.fib(10000))
      (f2, t2) <- measure(StackSafety.fib2(10000))
      _ <- IO(println(s"fib1: $f1 time: $t1"))
      _ <- IO(println(s"fib2: $f2 time: $t2"))
    } yield ExitCode.Success

  private def measure[F[_], A](fa: F[A])(implicit F: Sync[F], clock: Clock[F]): F[(A, Long)] =
    for {
      start <- clock.monotonic(MILLISECONDS)
      result <- fa
      finish <- clock.monotonic(MILLISECONDS)
    } yield (result, finish - start)

}
