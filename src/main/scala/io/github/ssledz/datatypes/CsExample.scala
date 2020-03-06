package io.github.ssledz.datatypes

import java.util.concurrent.Executors

import cats.effect.IO

import scala.concurrent.ExecutionContext

object CsExample {

  def main(args: Array[String]): Unit = {

    val blocking = Executors.newCachedThreadPool()
    implicit val blockingEc = ExecutionContext.fromExecutor(blocking)

    val cs = IO.contextShift(concurrent.ExecutionContext.global)

    cs.evalOn(blockingEc) {
        foo {
          Thread.sleep(1000)
          println(s"[${Thread.currentThread().getName}] after io")
          "Result"
        }
      }
      .flatMap(res => IO(println(s"[${Thread.currentThread().getName}] result: $res")))
      .unsafeRunAsyncAndForget()

    println(s"[${Thread.currentThread().getName}] end")

    Thread.sleep(3000)

    blocking.shutdown()

  }

  def foo[A](a: => A)(implicit ec: ExecutionContext): IO[A] = IO.async { cb =>
    val task = new Runnable {
      override def run(): Unit = try { cb(Right(a)) } catch { case e => cb(Left(e)) }
    }
    ec.execute(task)
  }

}
