package com.teamlab.scala.konv

import com.teamlab.scala.konv.internal.CollectionMapper

import scala.language.experimental.macros
import com.teamlab.scala.konv.internal.NotSub._

trait Mapper[-A, B] { def map(a: A): B }

object Mapper {

  /** create Konv */
  def apply[A, B](f: A => B): Mapper[A, B] = f(_)

  /** auto generate Konv[A, B]{ a:A => Konv.to[B].by(a) }  */
  def mapper[A, B]: Mapper[A, B] = macro internal.Macro.generateMappingImpl[A, B]

  trait LowPriorityDefaults extends CollectionMapper {

    /** rule. convert option item */
    implicit def default_optionMap[A, B](
        implicit imap: Mapper[A, B],
        ev: A <:!< B
    ): Mapper[Option[A], Option[B]] = Mapper { a: Option[A] =>
      a.map(imap.map)
    }

    /** rule. convert object to option */
    implicit def default_toOption[A, B](
        implicit imap: Mapper[A, B]
    ): Mapper[A, Option[B]] = Mapper { a: A =>
      Option(imap.map(a))
    }
  }
  trait Defaults2 extends LowPriorityDefaults {
    implicit def default_useImplicitConversion[B, A](
        implicit ev: A => B
    ): Mapper[A, B] = Mapper { a =>
      ev(a)
    }
  }

  /** rules. same type through (not copy) */
  trait Defaults extends Defaults2 {
    implicit def default_extends[A <: B, B]: Mapper[A, B] = Mapper { a =>
      a
    }
  }
  trait UnsafeOption {
    implicit def optionToA[A] = Mapper[Option[A], A](_.get)
    implicit def optionToB[A, B](implicit m: Mapper[A, B]) = Mapper[Option[A], B](a => m.map(a.get))
  }
}
