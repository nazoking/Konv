package com.teamlab.scala.konv

import scala.annotation.tailrec

trait Config
object Config {
  final class Empty extends Config
  final class GetCode[C <: Config] extends Config
  final class OptionToNone[C <: Config] extends Config
  final class RecursiveAutoMapping[C <: Config] extends Config
  class ValueSet(values: Values.ValueSet) {
    def hasGetCode = values.contains(Values.GetCode)
    def hasOptionToNone = values.contains(Values.OptionToNone)
    def hasRecursiveAutoMapping = values.contains(Values.RecursiveAutoMapping)
    def toStringList() = values.toList.map(_.toString)
  }
  object Values extends Enumeration {
    val GetCode, OptionToNone, RecursiveAutoMapping = Value
  }
  @tailrec
  def parse(
      c: scala.reflect.macros.blackbox.Context
  )(value: c.Type, config: Values.ValueSet): Values.ValueSet = {
    if (value <:< c.typeOf[GetCode[_]]) {
      parse(c)(value.typeArgs.head, config + Values.GetCode)
    } else if (value <:< c.typeOf[OptionToNone[_]]) {
      parse(c)(value.typeArgs.head, config + Values.OptionToNone)
    } else if (value <:< c.typeOf[RecursiveAutoMapping[_]]) {
      parse(c)(value.typeArgs.head, config + Values.RecursiveAutoMapping)
    } else {
      config
    }
  }

}
