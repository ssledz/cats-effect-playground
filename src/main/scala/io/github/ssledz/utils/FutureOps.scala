package io.github.ssledz.utils

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object FutureOps {

  implicit class FutureSyntax[A](val fa : Future[A]) extends AnyVal {
    def get : A = Await.result(fa, Duration.Inf)
  }

}
