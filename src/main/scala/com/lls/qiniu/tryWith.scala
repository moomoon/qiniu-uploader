package com.lls.qiniu

import scala.util.Try

object tryWith {
  def apply[A <: AutoCloseable, B](resource: ⇒ A)(code: A ⇒ B): Try[B] = {
    val r = resource
    val tryResult = Try(code(r))
    r.close()
    tryResult
  }
}
