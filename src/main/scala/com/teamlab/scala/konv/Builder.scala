package com.teamlab.scala.konv

import scala.language.experimental.macros
import scala.language.dynamics

class Builder[T] extends Dynamic{
  def applyDynamicNamed(name: String)(args: (String, Any)*): T = macro KonvMacro.builldWithParamsImpl[T]
  def applyDynamic(name: String)(args:Any): T = macro KonvMacro.builldWithoutParamsImpl[T]
}
