package io.github.ssledz.typeclasses

import cats.data.EitherT
import cats.effect.{IO, LiftIO}
import cats.implicits._
import io.github.ssledz.utils.FutureOps._
import io.github.ssledz.utils.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
object LiftIOExample extends App {

  type MyEffect[A] = Future[Either[Throwable, A]]

  implicit def myEffectLiftIO: LiftIO[MyEffect] =
    new LiftIO[MyEffect] {
      def liftIO[A](ioa: IO[A]): MyEffect[A] = ioa.attempt.unsafeToFuture()
    }

  val ioa: IO[String] = IO("Hello World!")

  val effect: MyEffect[String] = LiftIO[MyEffect].liftIO(ioa)

  Logger.unsafeDbg("effect: " + effect.get)

  val L = implicitly[LiftIO[MyEffect]]

  val service1: MyEffect[Int] = Future.successful(Right(22))
  val service2: MyEffect[Boolean] = Future.successful(Right(true))
  val service3: MyEffect[String] = Future.successful(Left(new Exception("boom!")))

  val program: MyEffect[String] =
    (for {
      _ <- EitherT(service1)
      x <- EitherT(service2)
      y <- EitherT(if (x) L.liftIO(IO("from io")) else service3)
    } yield y).value

  Logger.unsafeDbg("program: " + program.get)

}
