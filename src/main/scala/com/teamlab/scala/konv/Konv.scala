package com.teamlab.scala.konv

import scala.language.experimental.macros

trait Konv[-A, B] { def map(a: A): B }

object Konv {

  /** create Konv */
  def apply[A, B](f: A => B): Konv[A, B] = f(_)

  /** auto generate Konv[A, B]{ a:A => Konv.to[B].by(a) }  */
  def mapper[A, B]: Konv[A, B] = macro internal.Macro.generateMappingImpl[A, B]
}
