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
  implicit class BoolOption(b: Boolean) {
    def toOption[A](f: => A): Option[A] = if (b) Some(f) else None
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
    by[TO](getConstructorFactory(typ), args, overwrites.toMap, ConfigValueSet(c.weakTypeOf[ConfigTag]))
  }

  def buildByFactory[TO: c.WeakTypeTag, ConfigTag: c.WeakTypeTag](factory: c.Tree): c.Expr[TO] =
    macroErrorGuard {
      val ftr = factory match {
        case q"({ (..$_) => $method(..$_) })" =>
          Factory(method, method.symbol.asMethod)
        case q"($obj) " =>
          val method = obj.tpe.member(TermName("apply"))
          Factory(obj, method.asMethod)
      }
      val (args, overwrites) = findArs(c.prefix.tree)
      by[TO](Right(ftr), args, overwrites.toMap, ConfigValueSet(c.weakTypeOf[ConfigTag]))
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
      config: Config.ValueSet
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
            val px = FactoryParameters(weakTypeOf[TO], fct.tree, fct.method, sources, overwrites, config)
            q"""{ ..${px.parameterNameDefines}; ${px.callTree()} }"""
          case Left(err) => throw err
        }
      }
    if (config.hasGetCode) {
      c.abort(c.prefix.tree.pos, s"$pkg.From.getCode !!!!!!!!!!!!!!\n ${show(ret)}")
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
      overwrites: Map[c.TermName, c.Tree] = Map.empty,
      config: Config.ValueSet = ConfigValueSet.empty()
  ) {
    case class Param(param: c.Symbol) {
      def name = param.name.toTermName
      def tpe = param.typeSignature.asSeenFrom(targetType, targetType.typeSymbol.asClass)
      def hasDefault = param.asTerm.isParamWithDefault
      def isOption = param.typeSignature.typeConstructor == c.typeOf[Option[_]].typeConstructor
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
    case class OptionToNone(param: Param) extends Parameter {
      override def tree: c.Tree = q"""${param.name} = None"""
    }
    case class RecursiveAutoMapping(param: Param, source: SourceInfo) extends Parameter with WithSource {
      def calls(x: c.Tree): List[TermName] = x match {
        case Select(pref, TermName(term)) => TermName(term) :: calls(pref)
        case _                            => Nil
      }
      override def tree: c.Tree =
        q"""${param.name.toTermName} = ($pkg.From(${source.name}.${param.name}).${TermName(
          calls(c.prefix.tree).reverse.mkString(".")
        )}).to[${param.tpe}] """
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
                    case Some(u)                                => Use(parameter, u, source)
                    case None if config.hasRecursiveAutoMapping => RecursiveAutoMapping(parameter, source)
                    case None                                   => Unmatched(parameter, source, sourceType)
                  }
              }
          } orElse
          (config.hasOptionToNone && parameter.isOption).toOption(OptionToNone(parameter)) orElse
          parameter.hasDefault.toOption(Default(parameter)) getOrElse
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
        q"""val ${s.name}:${s.tpe} = ${s.tree}"""
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
      // if is `TARGET extends SOURCE`
      (sourceType <:< targetType).toOption(source) orElse
      // if exists implicit conversion
      getImplicitConvert(source, sourceType, targetType) orElse
      // if can compose Mapper( exists all sub class mapper )`
      synthesisMapper(sourceType, targetType).map(mapper => q"$mapper.map($source)") orElse
      // if exists constructor `new TARGET(SOURCE)`
      findSingleValueConstructor(targetType, sourceType).map(_ => q"new $targetType($source)") orElse
      findSingleValueCaseClassParam(sourceType).flatMap { param =>
        // if `case class SOURCE(TARGET)` then SOURCE.TARGET
        (param.typeSignature <:< targetType).toOption(q"$source.${param.name.toTermName}") orElse
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
    Some(t).filter(_.nonEmpty)
  }
  def getImplicitConvert(source: c.Tree, sourceType: c.Type, targetType: c.Type): Option[c.Tree] = {
    val t = c.inferImplicitView(source, sourceType, targetType, silent = true)
    Some(t).filter(_.nonEmpty).map(t => q"$t($source)")
  }
  def synthesisMapper(sourceType: c.Type, targetType: c.Type): Option[c.Tree] = {
    val pName = TermName(c.freshName())
    val pNameTree = q"$pName"
    val clauses = knownDirectSubclasses(sourceType).toSeq.map { sub =>
      // SOURCE.SUB => TARGET
      autoConvert(pNameTree, sub.asType.toType, targetType)
        .orElse {
          knownDirectSubclasses(targetType).find(_.name == sub.name).flatMap { ts =>
            // same name
            // can convert SOURCE.SUB => TARGET.SUB
            autoConvert(pNameTree, sub.asType.toType, ts.asType.toType) orElse
              // SOURCE.SUB => TARGET.SUB ( TARGET.SUB is object)
              ts.isModuleClass.toOption(q"${ts.asClass.module}") orElse
              // SOURCE.SUB => new TARGET.SUB(x=SOURCE.SUB.x, y=SOURCE.SUB.y, ...)
              getConstructorFactory(ts.asType.toType).toOption.flatMap { f =>
                val src = Seq(SourceInfo(pNameTree, sub.asType.toType, pName))
                val fParams = FactoryParameters(ts.asType.toType, f.tree, f.method, src)
                (!fParams.hasError).toOption(fParams.callTree())
              }
          }
        }
        .map { tree =>
          cq""" $pName:${sub.asType.toType} => $tree """
        }
    }
    (clauses.nonEmpty && clauses.forall(_.isDefined))
      .toOption(q""" $pkg.Mapper[$sourceType, $targetType]{ case ..${clauses.flatten} } """)
  }
  def knownDirectSubclasses(tpe: c.Type): Set[c.Symbol] = {
    if (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isSealed) {
      tpe.typeSymbol.asClass.knownDirectSubclasses
    } else Set.empty
  }

  def findSingleValueConstructor(t: c.Type, o: c.Type): Option[c.Symbol] = {
    t.typeConstructor
      .member(c.universe.termNames.CONSTRUCTOR)
      .alternatives
      .filter(_.isPublic)
      .find(_.asMethod.paramLists match {
        case List(List(p)) => o <:< p.typeSignature
        case _             => false
      })
  }
  def findSingleValueCaseClassParam(t: c.Type): Option[c.Symbol] = {
    (t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass).toOption(true).flatMap { _ =>
      t.typeConstructor.decls
        .collectFirst {
          case m: MethodSymbol if m.isPrimaryConstructor => m.paramLists.head
        }
        .collectFirst { case p :: Nil => p }
    }
  }
  object ConfigValueSet {
    import Config._
    def apply(value: c.Type): ValueSet = new ValueSet(Config.parse(c)(value, Values.ValueSet()))
    def empty() = new ValueSet(Values.ValueSet())
  }
}
