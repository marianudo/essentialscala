package com.codinginflipflops.essentialscala

object DSL {
  def withTimeout(milis: Long, f: => Unit): Unit = {
    Thread.sleep(milis)
    f
  }

  def ?[A](b: Boolean)(t: => A)(f: => A): A =
    if(b) t else f
}
