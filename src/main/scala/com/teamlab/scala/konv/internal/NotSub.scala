package com.teamlab.scala.konv.internal

object NotSub {
  trait <:!<[A, B]
  private val `_<:!<` : <:!<[Any, Any] = new <:!<[Any, Any] {}
  implicit def nsub[A, B]: A <:!< B = `_<:!<`.asInstanceOf[<:!<[A, B]]
  implicit def nsubAmbig1[A, B >: A]: A <:!< B = sys.error("Unexpected call")
  implicit def nsubAmbig2[A, B >: A]: A <:!< B = sys.error("Unexpected call")
}
