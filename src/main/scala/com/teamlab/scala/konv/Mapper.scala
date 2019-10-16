package com.teamlab.scala.konv

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.language.experimental.macros

import com.teamlab.scala.konv.internal.NotSub.<:!<

trait Mapper[-A, B] { def map(a: A): B }

object Mapper {

  /** create Konv */
  def apply[A, B](f: A => B): Mapper[A, B] = f(_)

  /** auto generate Konv[A, B]{ a:A => Konv.to[B].by(a) }  */
  def mapper[A, B]: Mapper[A, B] = macro internal.Macro.generateMappingImpl[A, B]

  trait LowPriorityDefaults {

    /** rule. convert option item */
    implicit def default_optionMap[A, B](
        implicit imap: Mapper[A, B],
        ev: A <:!< B
    ): Mapper[Option[A], Option[B]] = Mapper { a: Option[A] =>
      a.map(imap.map)
    }

    /** rule. convert iterable type */
    implicit def default_listTo[C1, X, Col[_]](
        implicit cbf: CanBuildFrom[Nothing, X, Col[X]],
        ev1: C1 <:< Iterable[X],
        ev2: C1 <:!< Col[X]
    ): Mapper[C1, Col[X]] = _.to[Col]

    /** rule. convert iterable items */
    implicit def default_iterable[X, Y, CX[X] <: Iterable[X], CY[Y] <: Iterable[
      Y
    ]](
        implicit cmap: Mapper[Iterable[Y], CY[Y]],
        imap: Mapper[X, Y],
        ev: CX[X] <:!< CY[Y]
    ): Mapper[CX[X], CY[Y]] = Mapper { a: CX[X] =>
      cmap.map(a.map(imap.map))
    }

    /** rule. convert map values */
    implicit def default_mapValues[A, B, X](
        implicit imap: Mapper[A, B],
        ev1: Map[X, A] <:!< Map[X, B],
        ev2: A <:!< B
    ): Mapper[Map[X, A], Map[X, B]] = Mapper { a: Map[X, A] =>
      a.mapValues(imap.map)
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
}
