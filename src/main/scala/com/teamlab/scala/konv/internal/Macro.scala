package com.teamlab.scala.konv.internal

import scala.reflect.api.{Position => Pos}
import scala.reflect.macros.blackbox.Context

import com.teamlab.scala.konv.{Config, From, Mapper}

class Macro(val c: Context) {
  import c.universe._
  val pkg = q"com.teamlab.scala.konv"
  private class MacroError(message: String, var pos: Option[Pos] = None) extends Exception(message) {
    def this(message: String, pos: Pos) = this(message, Some(pos))
  }
  private def macroErrorGuard[TO](f: => c.Expr[TO]): c.Expr[TO] = {
    try f
    catch {
      case e: MacroError =>
        c.error(
          e.pos.getOrElse(c.macroApplication.pos).asInstanceOf[c.Position],
          e.getMessage
        )
        c.Expr[TO](q"")
    }
  }
  def generateMappingImpl[FROM: c.WeakTypeTag, TO: c.WeakTypeTag]: c.Expr[Mapper[FROM, TO]] = macroErrorGuard {
    val sourceType = weakTypeOf[FROM]
    val targetType = weakTypeOf[TO]
//    val to = if (targetType.typeSymbol.isClass && targetType.typeSymbol.asClass.isCaseClass) {
//      q"to[${targetType}]"
//    } else {
//      q"to[$targetType]"
//    }
    //    println(q"""Mapper[$sourceType, $targetType]{ a:$sourceType => $to.by(a) }""")
    c.Expr(q"""$pkg.Mapper[$sourceType, $targetType]{
         from:$sourceType => $pkg.From(from).to[$targetType] }""")
  }
  case class SourceInfo(tree: c.Tree, tpe: c.Type, name: c.TermName)
  object SourceInfo {
    def apply(tree: c.Tree): SourceInfo = new SourceInfo(tree, tree.tpe, TermName(c.freshName()))
  }

  case class Factory(tree: Tree, method: MethodSymbol)
  def buildByConstructor[TO: c.WeakTypeTag, ConfigTag: c.WeakTypeTag]: c.Expr[TO] = macroErrorGuard {
    val typ = weakTypeOf[TO]
    val (args, overwrites) = findArs(c.prefix.tree)
    by[TO](getConstructorFactory(typ), args, overwrites.toMap, ConfigValue.parse(c.weakTypeOf[ConfigTag]))
  }
  object ConfigValue extends Enumeration {
    val GetCode = Value
    def parse(
        value: c.Type,
        config: ConfigValue.ValueSet = ConfigValue.ValueSet()
    ): ConfigValue.ValueSet = {
      if (value.typeConstructor == c.typeOf[Config.GetCode[_]].typeConstructor) {
        parse(value.typeArgs.head, config + ConfigValue.GetCode)
      } else
        config
    }
  }

  def buildByFactory[TO: c.WeakTypeTag, ConfigTag: c.WeakTypeTag](factory: c.Tree): c.Expr[TO] =
    macroErrorGuard {
//      val config = parseConfig(c.weakTypeOf[ConfigTag])
      val ftr = factory match {
        case q"({ (..$_) => $method(..$_) })" =>
          Factory(method, method.symbol.asMethod)
        case q"($obj) " =>
          val method = obj.tpe.member(TermName("apply"))
          Factory(obj, method.asMethod)
      }
      val (args, overwrites) = findArs(c.prefix.tree)
      by[TO](Right(ftr), args, overwrites.toMap, ConfigValue.parse(c.weakTypeOf[ConfigTag]))
    }
  private def findArs(
      prefix: c.Tree
  ): (Seq[c.Tree], Seq[(c.TermName, c.Tree)]) =
    prefix
      .collect {
        case q"""$from.applyDynamicNamed("apply")(..$named)""" if from.tpe =:= typeOf[From[Config.Empty]].companion =>
          val (args, overwrites) = named
            .collect {
              case q"($key, $value)" => TermName(literalString(key)) -> value
            }
            .partition(_._1.toString.isEmpty)
          args.map(_._2) -> overwrites
        case q"""$from.applyDynamic("apply")(..$args)""" if from.tpe =:= typeOf[From[Config.Empty]].companion =>
          args -> Seq()
      }
      .headOption
      .getOrElse(
        throw new MacroError(s"no match From.apply ${showRaw(c.prefix.tree)}")
      )

  private def by[TO: c.WeakTypeTag](
      factory: Either[Exception, Factory],
      args: Seq[c.Tree],
      overwrites: Map[c.TermName, c.Tree],
      config: ConfigValue.ValueSet
  ): c.Expr[TO] = {
    val sources = args.map(SourceInfo.apply)
    val ret = sources.headOption
      .flatMap { s =>
        val ret = autoConvert(s.tree, s.tpe, weakTypeOf[TO])
        if (ret.isDefined) {
          (args.drop(1) ++ overwrites.toSeq.map(_._2)).headOption.foreach { op =>
            throw new MacroError(s"arguments use only first", op.pos)
          }
        }
        ret
      }
      .getOrElse {
        factory match {
          case Right(fct) =>
            val px = FactoryParameters(weakTypeOf[TO], fct.tree, fct.method, sources, overwrites)
            q"""{ ..${px.parameterNameDefines}; ${px.callTree()} }"""
          case Left(err) => throw err
        }
      }
    if (config.contains(ConfigValue.GetCode)) {
      c.abort(c.prefix.tree.pos, s"$pkg.From.getCode !!!!!!!!!!!!!!\n ${showCode(ret)}")
    }
//    println(ret)
    c.Expr[TO](ret)
  }

  private def getConstructorFactory(typ: c.Type) = {
    val constructor = typ.member(termNames.CONSTRUCTOR)
    constructor.alternatives.filter(_.asMethod.isPublic) match {
      case head :: Nil => Right(Factory(q"$typ", head.asMethod))
      case Nil         => Left(new MacroError(s"Cant'convert. ${typ} has not public constructor"))
      case list =>
        list.find(_.asMethod.isPrimaryConstructor) match {
          case Some(cst) =>
            Right(Factory(q"$typ", cst.asMethod))
          case None =>
            Left(new MacroError(s"Cant'convert. ${typ} has not primary constructor. ${list.size} constructors exists"))
        }
    }
  }
  private def literalString: PartialFunction[c.Tree, String] = {
    case Literal(Constant(str: String)) => str
  }

  case class FactoryParameters(
      targetType: c.Type,
      factoryTree: c.Tree,
      factory: MethodSymbol,
      sources: Seq[SourceInfo],
      overwrites: Map[c.TermName, c.Tree] = Map.empty
  ) {
    case class Param(param: c.Symbol) {
      def name = param.name.toTermName
      def tpe = param.typeSignature.asSeenFrom(targetType, targetType.typeSymbol.asClass)
      def hasDefault = param.asTerm.isParamWithDefault
    }
    sealed trait Parameter {
      def param: Param
      def tree: c.Tree = EmptyTree
    }
    trait WithSource {
      def source: SourceInfo
    }
    case class Overwrite(param: Param, overwrite: c.Tree) extends Parameter {
      override def tree = q"""${param.name} = $overwrite"""
    }
    case class Use(param: Param, value: c.Tree, source: SourceInfo) extends Parameter with WithSource {
      override def tree = q"""${param.name} = $value"""
    }
    case class Default(param: Param) extends Parameter {}
    case class Unmatched(param: Param, source: SourceInfo, sourceType: c.Type) extends Parameter with WithSource {
      override def tree = q"""${param.name} = ${source.name}.${param.name}"""
    }
    case class NotFound(param: Param) extends Parameter {}

    val params: List[Parameter] = factory.paramLists.head
      .map(Param)
      .map { parameter =>
        overwrites.get(parameter.name).map(value => Overwrite(parameter, value)) orElse
          findSourceField(sources, parameter.name).map {
            case (source, method) =>
              val sourceType = method.returnType.asSeenFrom(source.tpe, source.tpe.typeSymbol.asClass)
              autoConvert(q"${source.name}.${parameter.name}", sourceType, parameter.tpe) match {
                case Some(v) => Use(parameter, v, source)
                case None =>
                  getImplicitConvert(q"${source.name}.${parameter.name}", sourceType, parameter.tpe) match {
                    case Some(u) => Use(parameter, u, source)
                    case None    => Unmatched(parameter, source, sourceType)
                  }
              }
          } orElse
          (if (parameter.hasDefault) Some(Default(parameter)) else None) getOrElse
          NotFound(parameter)
      }
    lazy val errorMessages: List[String] = params.collect {
      case x: Unmatched =>
        s"unmatched type found ${x.source.tree}.${x.param.name}:${x.sourceType}, required ${x.param.name}:${x.param.tpe}"
      case x: NotFound => s"unspecified value parameter ${x.param.name}:${x.param.tpe}"
    } ++ unusedOverwrites.toList.map {
      case (key, tree) => s"unused parameter ${key}=${tree}"
    }
    def hasError: Boolean = errorMessages.nonEmpty
    def unusedOverwrites = {
      overwrites.removedAll(params.collect { case p: Overwrite => p.param.name })
    }
    def factorySig: String =
      s"${factory.fullName}(${factory.paramLists.head.map(p => s"${p.name}:${p.typeSignature}").mkString(", ")})"
    def callTree(silent: Boolean = false): c.Tree = {
      val tree = if (factory.isConstructor) {
        q""" new $factoryTree(..${params.map(_.tree).filter(_.nonEmpty)}) """
      } else {
        q""" $factoryTree(..${params.map(_.tree).filter(_.nonEmpty)}) """
      }
      if (!silent && hasError) {
        var t2 = tree.toString()
        params.collect { case c: WithSource => c.source }.toSet.foreach { s: SourceInfo =>
          t2 = t2.replaceAllLiterally(s.name.toString, s.tree.toString)
        }
        throw new MacroError(s"Cant'convert ${factorySig}\n  ${errorMessages.mkString("\n  ")}\n   in $t2")
      }
      tree
    }
    def parameterNameDefines: Set[c.Tree] =
      params.collect { case c: WithSource => c.source }.toSet.map { s: SourceInfo =>
        q"""val ${s.name} = ${s.tree}"""
      }
  }
  def findSourceField(sources: Seq[SourceInfo], name: TermName): Option[(SourceInfo, c.universe.MethodSymbol)] =
    (for {
      src <- sources.view
      mem = src.tpe.member(name) if mem != NoSymbol
      alts <- mem.alternatives.view if alts.isMethod
      m2 = alts.asMethod
      if m2.paramLists.isEmpty || (m2.paramLists.size == 1 && m2.paramLists.head.isEmpty)
    } yield {
      (src, m2)
    }).headOption

  def autoConvert(
      source: c.Tree,
      sourceType: c.Type,
      targetType: c.Type
  ): Option[c.Tree] =
    // if exists `implicit Mapper[SOURCE, TARGET]`
    getImplicitMapperValue(sourceType, targetType).map(mapper => q"$mapper.map($source)") orElse
      // if exists `implicit Mapper[SOURCE, TARGET]`
      synthesisMapper(sourceType, targetType).map(mapper => q"$mapper.map($source)") orElse
      // if is `TARGET extends SOURCE`
      (if (sourceType <:< targetType) Some(source) else None) orElse
      getImplicitConvert(source, sourceType, targetType) orElse
      // if exists constructor `new TARGET(SOURCE)`
      findSingleValueConstructor(targetType, sourceType).map(_ => q"new $targetType($source)") orElse
      findSingleValueCaseClassParam(sourceType).flatMap { param =>
        // if `case class SOURCE(TARGET)` then SOURCE.TARGET
        (if (param.typeSignature <:< targetType) Some(q"$source.${param.name.toTermName}") else None) orElse
          // if `case class SOURCE(PARAM1)` and `new TARGET(SOURCE.PARAM1)`
          findSingleValueConstructor(targetType, param.typeSignature)
            .map(_ => q"new $targetType($source.${param.name.toTermName})")
      }
  def getImplicitMapperValue(sourceType: c.Type, targetType: c.Type): Option[c.Tree] = {
    val t = c.inferImplicitValue(
      appliedType(typeOf[Mapper[_, _]], List(sourceType, targetType)),
      silent = true,
      withMacrosDisabled = false
    )
    if (t.isEmpty) None else Some(t)
  }
  def getImplicitConvert(source: c.Tree, sourceType: c.Type, targetType: c.Type): Option[c.Tree] = {
    val t = c.inferImplicitView(source, sourceType, targetType, silent = true)
    if (t.isEmpty) None else Some(q"$t($source)")
  }
  def synthesisMapper(sourceType: c.Type, targetType: c.Type): Option[c.Tree] = {
    val pName = TermName(c.freshName())
    val pNameTree = q"$pName"
    val clauses = knownDirectSubclasses(sourceType).toSeq.map { sub =>
      autoConvert(pNameTree, sub.asType.toType, targetType)
        .orElse {
          knownDirectSubclasses(targetType).find(_.name == sub.name).flatMap { ts =>
            autoConvert(pNameTree, sub.asType.toType, ts.asType.toType).orElse {
              getConstructorFactory(ts.asType.toType).right.toOption.flatMap { f =>
                val fParams =
                  FactoryParameters(
                    ts.asType.toType,
                    f.tree,
                    f.method,
                    Seq(SourceInfo(pNameTree, sub.asType.toType, pName))
                  )
                if (!fParams.hasError) Some(fParams.callTree()) else None
              }
            }
          }
        }
        .map { tree =>
          cq""" $pName:${sub.asType.toType} => $tree """
        }
    }
    if (clauses.nonEmpty && clauses.forall(_.isDefined)) {
      Some(q""" $pkg.Mapper[$sourceType, $targetType]{ case ..${clauses.flatten} } """)
    } else None
  }
  def knownDirectSubclasses(tpe: c.Type): Set[c.Symbol] = {
    if (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isSealed) {
      tpe.typeSymbol.asClass.knownDirectSubclasses
    } else Set.empty
  }

  def findSingleValueConstructor(t: c.Type, o: c.Type) = {
    t.typeConstructor
      .member(c.universe.termNames.CONSTRUCTOR)
      .alternatives
      .filter(_.isPublic)
      .find(_.asMethod.paramLists match {
        case List(List(p)) => o <:< p.typeSignature
        case _             => false
      })
  }
  def findSingleValueCaseClassParam(t: c.Type) = {
    if (t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass) {
      t.typeConstructor.decls
        .collectFirst {
          case m: MethodSymbol if m.isPrimaryConstructor => m
        }
        .map(_.paramLists.head) match {
        case Some(p :: Nil) => Some(p)
        case _              => None
      }
    } else {
      None
    }
  }
}
