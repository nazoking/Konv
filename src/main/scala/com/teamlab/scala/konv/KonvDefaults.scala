package com.teamlab.scala.konv

import scala.collection.generic.CanBuildFrom

/**
  * default convert rules.
  */
object KonvDefaults {
  trait <:!<[A, B]
  private val `_<:!<` : <:!<[Any, Any] = new <:!<[Any, Any] {}
  implicit def nsub[A, B]: A <:!< B = `_<:!<`.asInstanceOf[<:!<[A, B]]
  implicit def nsubAmbig1[A, B >: A]: A <:!< B = sys.error("Unexpected call")
  implicit def nsubAmbig2[A, B >: A]: A <:!< B = sys.error("Unexpected call")
  trait LowPriorityDefaults {

    /** rule. convert option item */
    implicit def default_optionMap[A, B](
        implicit imap: Konv[A, B],
        ev: A <:!< B
    ): Konv[Option[A], Option[B]] = Konv { a: Option[A] =>
      a.map(imap.map)
    }

    /** rule. convert iterable type */
    implicit def default_listTo[C1, X, Col[_]](
        implicit cbf: CanBuildFrom[Nothing, X, Col[X]],
        ev1: C1 <:< Iterable[X],
        ev2: C1 <:!< Col[X]
    ): Konv[C1, Col[X]] = _.to[Col]

    /** rule. convert iterable items */
    implicit def default_iterable[X, Y, CX[X] <: Iterable[X], CY[Y] <: Iterable[
      Y
    ]](
        implicit cmap: Konv[Iterable[Y], CY[Y]],
        imap: Konv[X, Y],
        ev: CX[X] <:!< CY[Y]
    ): Konv[CX[X], CY[Y]] = Konv { a: CX[X] =>
      cmap.map(a.map(imap.map))
    }

    /** rule. convert map values */
    implicit def default_mapValues[A, B, X](
        implicit imap: Konv[A, B],
        ev1: Map[X, A] <:!< Map[X, B],
        ev2: A <:!< B
    ): Konv[Map[X, A], Map[X, B]] = Konv { a: Map[X, A] =>
      a.mapValues(imap.map)
    }

    /** rule. convert object to option */
    implicit def default_toOption[A, B](
        implicit imap: Konv[A, B]
    ): Konv[A, Option[B]] = Konv { a: A =>
      Option(imap.map(a))
    }
  }
  trait Defaults2 extends LowPriorityDefaults {
    implicit def default_useImplicitConversion[B, A](
        implicit ev: A => B
    ): Konv[A, B] = Konv { a =>
      ev(a)
    }
  }

  /** rules. same type through (not copy) */
  trait Defaults extends Defaults2 {
    implicit def default_extends[A <: B, B]: Konv[A, B] = Konv { a =>
      a
    }
  }
}

trait KonvDefaults extends KonvDefaults.Defaults
