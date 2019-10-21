package com.teamlab.scala.konv.internal

import scala.language.higherKinds
import com.teamlab.scala.konv.Mapper
import com.teamlab.scala.konv.internal.NotSub.<:!<

import scala.collection.generic.CanBuildFrom

trait CollectionMapper {

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
}
