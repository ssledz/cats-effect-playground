package io.github.ssledz.concurrencies

import cats.effect._
import cats.effect.concurrent._

object MVarExample extends IOApp {
  // MVars are mutable references that can be used to share mutable state between threads.
  // An MVar has two states empty and full.
  // - Reading from an empty MVar will block the current thread.
  // - Writing to a full MVar will also block the current thread.
  // Use-cases:
  // - As synchronized, thread-safe mutable variables
  // - As channels, with take and put acting as “receive” and “send”
  // - As a binary semaphore, with take and put acting as “acquire” and “release”

  // It has these fundamental, atomic operations:
  // - *put* which fills the var if empty, or blocks (asynchronously) until the var is empty again
  // - *take* which empties the var if full, returning the contained value,
  //   or blocks (asynchronously) otherwise until there is a value to pull
  // - *read* which reads the current value without touching it, assuming there is one, or otherwise it waits until a value is made available via put
  // - *isEmpty* returns true if currently empty
  // - *tryPut* which fills the var if empty; returns true if successful
  // - *tryTake* empties if full, returns None if empty.
  // - *tryRead* returns Some(a) if full, without modifying the var, or else returns None

  def sum(state: MVar[IO, Int], list: List[Int]): IO[Int] =
    list match {
      case Nil => state.take
      case x :: xs =>
        for {
          current <- state.take
          _ <- state.put(current + x)
          s <- sum(state, xs)
        } yield s
    }

  val program1 =
    for {
      state <- MVar[IO].of(0)
      r <- sum(state, (0 to 10).toList)
    } yield r


  // Async Lock
  final class MLock(mvar: MVar[IO, Unit]) {
    def acquire: IO[Unit] =
      mvar.take

    def release: IO[Unit] =
      mvar.put(())

    def greenLight[A](fa: IO[A]): IO[A] =
      acquire.bracket(_ => fa)(_ => release)
  }

  object MLock {
    def apply(): IO[MLock] =
      MVar[IO].of(()).map(v => new MLock(v))
  }

  val program2 =
    for {
      lock <- MLock()
      state <- MVar[IO].of(0)
      task = sum(state, (0 until 100).toList)
      r <- lock.greenLight(task)
    } yield r

  // producer consumer model
  type Channel[A] = MVar[IO, Option[A]]

  def monotonicID =
    System.nanoTime().toString.takeRight(6)

  def producer(ch: Channel[Int], list: List[Int]): IO[Unit] = {
    println(s"[${monotonicID}] Producing ${list.headOption}, is channel empty: ${ch.isEmpty.unsafeRunSync}")
    list match {
      case Nil =>
        ch.put(None) // we are done!
      case head :: tail =>
        ch.put(Some(head)).flatMap { _ =>
          println(s"[${monotonicID}] Producing $head")
          producer(ch, tail)
        }
    }
  }

  def consumer(ch: Channel[Int], sum: Long): IO[Long] = {
    println(s"[${monotonicID}] Consuming ${ch.isEmpty.unsafeRunSync}")
    ch.take.flatMap {
      case Some(x) =>
        consumer(ch, sum + x)
      case None =>
        IO.pure(sum) // we are done!
    }
  }

  val program3 = for {
    channel <- MVar[IO].of(Option(0))
    count = 3
    producerTask = producer(channel, (0 until count).toList)
    consumerTask = consumer(channel, 0L)

    fp  <- producerTask.start
    fc  <- consumerTask.start
    _   <- fp.join
    sum <- fc.join
  } yield sum

  def run(args: List[String]): IO[ExitCode] = for {
    //res <- program1
    // res <- program2
     res <- program3
    _ = println(res)
  } yield ExitCode.Success
}
