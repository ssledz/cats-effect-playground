package io.github.ssledz.concurrencies

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.concurrent.Deferred
import cats.implicits._

object DeferredExample extends IOApp {
  // Let's start from quick definition:
  // Deferred is *pure* functional primitive which represent single value which may *not* yet be available.
  // What it means, when created, Deferred is empty. It can be completed exactly once and never be empty again.
  // Deferred has two methods: get and complete. We should focus on expected 'calling behaviour'
  // get:
  // - *get* on an empty *Deferred* will block until the *Deferred* is completed
  // - *get* on a completed *Deferred* will always immediately return its content
  // - *get* is cancelable if (and):
  //    - F[_] implements *Concurrent*
  //    - *Deferred* value was built via the normal apply (and not via uncancelable)
  //   NOTE: Cancel operation that’s possible for as long as the Deferred value isn’t complete.
  //
  // complete:
  // - complete(a) on an empty Deferred will set it to a, and notify any and all readers currently blocked on a call to get.
  // - complete(a) on a Deferred that has already been completed will not modify its content, and *result in a failed F*.

  // Deferred can be used in conjunction with Ref to build complex concurrent behaviour and data structures like queues and semaphores.
  // IMPORTANT!: The blocking mentioned above is semantic only, no actual threads are blocked by the implementation.

  def start(d: Deferred[IO, Int]): IO[Unit] = {
    val attemptCompletion: Int => IO[Unit] = n => d.complete(n).attempt.void

    List(
      IO.race(attemptCompletion(1), attemptCompletion(2)),
      d.get.flatMap { n => IO(println(show"Result: $n")) }
    ).parSequence.void
  }

  val program: IO[Unit] =
    for {
      d <- Deferred[IO, Int]
      _ <- start(d)
    } yield ()

  def run(args: List[String]): IO[ExitCode] =
    (1 to 123).toList.traverse(_ => program).as(ExitCode.Success)

}
