package com.teamlab.scala.konv

import scala.language.experimental.macros
import scala.language.dynamics
import com.teamlab.scala.konv.internal.Macro

class From[ConfigTag <: Config] private[From] (val args: Seq[(String, Any)]) {

  /** get generated code as compile error */
  def getCode: From[Config.GetCode[ConfigTag]] = new From[Config.GetCode[ConfigTag]](args)
  def enableOptionDefaultsToNone: From[Config.OptionToNone[ConfigTag]] = new From[Config.OptionToNone[ConfigTag]](args)
  def recursiveAutoMapping: From[Config.RecursiveAutoMapping[ConfigTag]] =
    new From[Config.RecursiveAutoMapping[ConfigTag]](args)

  def to[TO]: TO = macro Macro.buildByConstructor[TO, ConfigTag]

  def to[TO](factory: (_) => TO): TO = macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](factory: (_, _) => TO): TO = macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](factory: (_, _, _) => TO): TO = macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](factory: (_, _, _, _) => TO): TO =
    macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](factory: (_, _, _, _, _) => TO): TO =
    macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](factory: (_, _, _, _, _, _) => TO): TO =
    macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](factory: (_, _, _, _, _, _, _) => TO): TO =
    macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](factory: (_, _, _, _, _, _, _, _) => TO): TO =
    macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](factory: (_, _, _, _, _, _, _, _, _) => TO): TO =
    macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](factory: (_, _, _, _, _, _, _, _, _, _) => TO): TO =
    macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](factory: (_, _, _, _, _, _, _, _, _, _, _) => TO): TO =
    macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](factory: (_, _, _, _, _, _, _, _, _, _, _, _) => TO): TO =
    macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](factory: (_, _, _, _, _, _, _, _, _, _, _, _, _) => TO): TO =
    macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _) => TO): TO =
    macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => TO): TO =
    macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => TO): TO =
    macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](
      factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => TO
  ): TO = macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](
      factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => TO
  ): TO = macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](
      factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => TO
  ): TO = macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](
      factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => TO
  ): TO = macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](
      factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => TO
  ): TO = macro Macro.buildByFactory[TO, ConfigTag]
  def to[TO](
      factory: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => TO
  ): TO = macro Macro.buildByFactory[TO, ConfigTag]
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
  def applyDynamicNamed(name: String)(args: (String, Any)*): From[Config.Empty] = {
    name match {
      case "apply" => new From[Config.Empty](args)
      case _       => throw new RuntimeException(s"method $name $args is not implemented")
    }
  }
  def applyDynamic(name: String)(args: Any*): From[Config.Empty] = {
    name match {
      case "apply" => new From[Config.Empty](args.map("" -> _))
      case _       => throw new RuntimeException(s"method $name $args is not implemented")
    }
  }
}
