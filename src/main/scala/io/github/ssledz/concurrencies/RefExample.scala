package io.github.ssledz.concurrencies

import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.effect.concurrent.Ref
import cats.implicits._

object RefExample extends IOApp {

  // Provides safe concurrent access and modification of its content, but no functionality for synchronisation, which is instead handled by Deferred.
  // For this reason, a Ref is always initialised to a value! CANNOT BE EMPTY
  // The default implementation is nonblocking and lightweight, consisting essentially of a purely functional wrapper over an AtomicReference.

  class Worker[F[_]](number: Int, ref: Ref[F, Int])(implicit F: Sync[F]) {
    private def putStrLn(value: String): F[Unit] = F.delay(println(value))

    def start: F[Unit] =
      for {
        c1 <- ref.get
        _  <- putStrLn(show"#$number >> $c1")
        c2 <- ref.modify(x => (x + 1, x))
        _  <- putStrLn(show"#$number >> $c2")
      } yield ()
  }

  val program: IO[Unit] =
    for {
      ref <- Ref.of[IO, Int](0)
      w1  = new Worker[IO](1, ref)
      w2  = new Worker[IO](2, ref)
      w3  = new Worker[IO](3, ref)
      _   <- List(
        w1.start,
        w2.start,
        w3.start
      ).parSequence.void
    } yield ()

  def run(args: List[String]): IO[ExitCode] =
    program.as(ExitCode.Success)
}
