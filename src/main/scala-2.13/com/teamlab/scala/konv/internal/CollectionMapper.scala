package com.teamlab.scala.konv.internal

import com.teamlab.scala.konv.Mapper
import com.teamlab.scala.konv.internal.NotSub._

import scala.collection.IterableFactory
import scala.language.higherKinds
import scala.reflect.runtime.{currentMirror, universe}

trait CollectionMapper {

  /** rule. convert iterable type */
  implicit def default_listTo[A, Y[A] <: Iterable[A], Z[A] <: Iterable[A]](
      implicit w: universe.WeakTypeTag[Z[_]],
      ev: Y[_] <:!< Z[_]
  ): Mapper[Y[A], Z[A]] = {
    val iterableFactory =
      currentMirror.reflectModule(w.tpe.typeSymbol.companion.asModule).instance.asInstanceOf[IterableFactory[Z]]
    i => i.to(iterableFactory)
  }

  /** rule. convert iterable items */
  implicit def default_iterable[X, Y, CX[X] <: Iterable[X], CY[Y] <: Iterable[Y]](
      implicit cMap: Mapper[Iterable[Y], CY[Y]],
      eMap: Mapper[X, Y],
      ev1: CX[X] <:!< CY[Y],
      ev2: X <:!< Y
  ): Mapper[CX[X], CY[Y]] = Mapper { a: CX[X] =>
    cMap.map(a.map(eMap.map))
  }

  /** rule. convert map values */
  implicit def default_mapValues[A, B, X](
      implicit iMap: Mapper[A, B],
      ev1: Map[X, A] <:!< Map[X, B],
      ev2: A <:!< B
  ): Mapper[Map[X, A], Map[X, B]] = Mapper { a: Map[X, A] =>
    a.view.mapValues(iMap.map).toMap
  }
}
