package com.teamlab.scala.konv

import scala.language.experimental.macros
import scala.language.dynamics

class From private[From] (val args: Seq[(String, Any)]) {

  def to[A]: A = macro internal.Macro.buildByConstructor[A]

  def to[A](factory: From => A): A = ???
  def to[A](factory: (_) => A): A = macro internal.Macro.buildByFactory[A]
  def to[A](factory: (_, _) => A): A = macro internal.Macro.buildByFactory[A]
  def to[A](factory: (_, _, _) => A): A = macro internal.Macro.buildByFactory[A]
  def to[A](factory: (_, _, _, _) => A): A =
    macro internal.Macro.buildByFactory[A]
  def to[A](factory: (_, _, _, _, _) => A): A =
    macro internal.Macro.buildByFactory[A]
  def to[A](factory: (_, _, _, _, _, _) => A): A =
    macro internal.Macro.buildByFactory[A]
  def to[A](factory: (_, _, _, _, _, _, _) => A): A =
    macro internal.Macro.buildByFactory[A]
  def to[A](factory: (_, _, _, _, _, _, _, _) => A): A =
    macro internal.Macro.buildByFactory[A]
  def to[A](factory: (_, _, _, _, _, _, _, _, _) => A): A =
    macro internal.Macro.buildByFactory[A]
  def to[A](factory: (_, _, _, _, _, _, _, _, _, _) => A): A =
    macro internal.Macro.buildByFactory[A]
  def to[A](factory: (_, _, _, _, _, _, _, _, _, _, _) => A): A =
    macro internal.Macro.buildByFactory[A]
  def to[A](factory: (_, _, _, _, _, _, _, _, _, _, _, _) => A): A =
    macro internal.Macro.buildByFactory[A]
  def to[A](factory: (_, _, _, _, _, _, _, _, _, _, _, _, _) => A): A =
    macro internal.Macro.buildByFactory[A]
  def to[A](factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A =
    macro internal.Macro.buildByFactory[A]
  def to[A](factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A =
    macro internal.Macro.buildByFactory[A]
  def to[A](factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A =
    macro internal.Macro.buildByFactory[A]
  def to[A](
      factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A
  ): A = macro internal.Macro.buildByFactory[A]
  def to[A](
      factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A
  ): A = macro internal.Macro.buildByFactory[A]
  def to[A](
      factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A
  ): A = macro internal.Macro.buildByFactory[A]
  def to[A](
      factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A
  ): A = macro internal.Macro.buildByFactory[A]
  def to[A](
      factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A
  ): A = macro internal.Macro.buildByFactory[A]
  def to[A](
      factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A
  ): A = macro internal.Macro.buildByFactory[A]
}

/**
  * {{{
  * From(a).to[TargetClass] => TargetClass
  * From(a, param1 = a.param2).to[TargetClass] => TargetClass
  * From(a).to(TargetClass.apply _) => TargetClass
  * From(a, param1=a.param2).to(TargetClass.apply _) => TargetClass
  * }}}
  */
object From extends Dynamic {
  def applyDynamicNamed(name: String)(args: (String, Any)*): From = {
    name match {
      case "apply" => new From(args)
      case _       => throw new RuntimeException(s"method $name $args is not implemented")
    }
  }
  def applyDynamic(name: String)(args: Any): From = {
    name match {
      case "apply" => new From(Seq("" -> args))
      case _       => throw new RuntimeException(s"method $name $args is not implemented")
    }
  }
}
