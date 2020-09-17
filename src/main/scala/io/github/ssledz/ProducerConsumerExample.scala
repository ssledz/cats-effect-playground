package io.github.ssledz

import java.util.concurrent.{BlockingQueue, Executors, LinkedBlockingDeque}

import cats._
import cats.effect.{Blocker, ContextShift, ExitCode, IO, IOApp, Resource, Timer}
import cats.implicits._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Random

object ProducerConsumerExample extends IOApp {

  val blockerResource: Resource[IO, Blocker] =
    Resource
      .make(IO(Executors.newCachedThreadPool()))(es => IO(es.shutdown()).handleErrorWith(_ => IO.unit))
      .map(es => Blocker.liftExecutionContext(ExecutionContext.fromExecutorService(es)))

  private def producer(service: ServerToServerNotificationService, size: Int): IO[Unit] =
    for {
      _ <- IO.unit
      _ <- IO(println(s"producing $size records"))
      _ <- IO((1 to size).toList.foreach(i => service.notifyAsync(s"http://www.wp.pl/$i")))
      _ <- IO.sleep(10000.millis)
    } yield ()

  def run(args: List[String]): IO[ExitCode] = blockerResource.use { blocker =>
    val service = new ServerToServerNotificationService(blocker)

    for {
      s <- service.startConsumers.as(ExitCode.Success).start
      p <- producer(service, 10).foreverM.start
      _ <- s.join
      _ <- p.join
    } yield ExitCode.Success

  }
}

class ServerToServerNotificationService(blocker: Blocker, capacity: Int = 20, numberOfConsumer: Int = 2) {

  private val queue: BlockingQueue[String] = new LinkedBlockingDeque[String](capacity)

  def notifyAsync(url: String): Unit =
    if (!queue.offer(url)) {
      println("Queue has no space left url: " + url)
    }

  private def doGet(consumer: String, url: String): IO[Unit] = IO {

    if (Random.nextBoolean()) {
      println(s"[${Thread.currentThread.getName}] $consumer error during call to " + url)
      throw new RuntimeException(s"something was wrong with $consumer")
    }

    val timeout = Random.nextInt(5000)

    println(s"[${Thread.currentThread.getName}] $consumer doing call [$timeout] to " + url)

    Thread.sleep(timeout)
  }

  def startConsumers(implicit cs: ContextShift[IO], timer: Timer[IO]): IO[Unit] = {

    def retry(value: IO[Unit])(n: Int): IO[Either[Int, String]] = value.attempt.flatMap {
      case Left(err) if n <= 0 => IO(println(s"!!!!!error: ${err.getMessage}")) *> IO(Right("was error: " + err.toString))
      case Left(err) => IO(println(s"trying again: $n, err: ${err.getMessage}")) *> IO(Left(n - 1))
      case _ => IO(println(s"Success n = $n")) *> IO(Right("done"))
    }

    def consumer(name: String): IO[Unit] =
      (for {
        url <- blocker.blockOn(IO(queue.take()))
        _ <- blocker.blockOn(doGet(name, url)).handleErrorWith(_ => IO(println(s"Retrying $url 3 times")) *> FlatMap[IO].tailRecM(3)(retry(blocker.blockOn(doGet(name, url)))))
      } yield ()).foreverM

    for {
      consumers <- (1 to numberOfConsumer).toList.map(i => consumer(s"consumer $i").start).parSequence
      _ <- consumers.map(_.join).sequence
    } yield ()

  }

}
