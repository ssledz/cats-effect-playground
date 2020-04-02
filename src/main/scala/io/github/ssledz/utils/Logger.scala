package io.github.ssledz.utils

import cats.effect.IO

object Logger {

  def unsafeDbg(msg: String): Unit = println(s"[${Thread.currentThread.getName}] $msg")

  def dbg(msg: String): IO[Unit] = IO(unsafeDbg(msg))

}
