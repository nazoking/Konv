package com.teamlab.scala.konv

import scala.language.experimental.macros

trait Konv[-A, B]{ def map(a:A):B }

/**
 * {{{
 * Konv.to[TargetClass].by(a) => TargetClass
 * Konv.to[TargetClass].by(a, param1 = a.param2) => TargetClass
 * Konv.to(TargetClass.apply _).by(a) => TargetClass
 * Konv.to(TargetClass.apply _).by(a, param1=a.param2) => TargetClass
 * }}}
 */
object Konv {
  /** create Konv */
  def apply[A, B](f: A => B):Konv[A, B] = f(_)
  /** auto generate Konv[A, B]{ a:A => Konv.to[B].by(a) }  */
  def mapper[A, B]:Konv[A, B] = macro KonvMacro.generateMappingImpl[A, B]
  def to[A]: Builder[A] = new Builder[A]()
  def to[A](factory:(_) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _, _, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _, _, _, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _, _, _, _, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
  def to[A](factory:(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A):Builder[A] = new Builder[A]()
}