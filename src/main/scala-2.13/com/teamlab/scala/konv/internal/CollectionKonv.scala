package com.teamlab.scala.konv.internal

import com.teamlab.scala.konv.Mapper
import com.teamlab.scala.konv.internal.NotSub._

import scala.collection.IterableFactory
import scala.language.higherKinds
import scala.reflect.runtime.{currentMirror, universe}

trait CollectionKonv {

  /** rule. convert iterable type */
  implicit def default_listTo[X, Y[X] <: Iterable[X], Z[X] <: Iterable[X]](
      implicit w: universe.WeakTypeTag[Z[_]],
      ev: Y[X] <:!< Z[X]
  ): Mapper[Y[X], Z[X]] = {
    val iterableFactory =
      currentMirror.reflectModule(w.tpe.typeSymbol.companion.asModule).instance.asInstanceOf[IterableFactory[Z]]
    i => i.to(iterableFactory)
  }

  /** rule. convert iterable items */
  implicit def default_iterable[X, Y, CX[X] <: Iterable[X], CY[Y] <: Iterable[Y]](
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
    a.view.mapValues(imap.map).toMap
  }
}
