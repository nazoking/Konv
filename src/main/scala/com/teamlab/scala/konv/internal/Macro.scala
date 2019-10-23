package com.teamlab.scala.konv.internal

import scala.reflect.api.{Position => Pos}
import scala.reflect.macros.blackbox.Context

import com.teamlab.scala.konv.{From, Mapper}

class Macro(val c: Context) {
  import c.universe._
  private class MacroError(message: String, var pos: Option[Pos] = None) extends Exception(message) {
    def this(message: String, pos: Pos) = this(message, Some(pos))
  }
  private def macroErrorGuard[B](f: => c.Expr[B]): c.Expr[B] = {
    try f
    catch {
      case e: MacroError =>
        c.error(
          e.pos.getOrElse(c.macroApplication.pos).asInstanceOf[c.Position],
          e.getMessage
        )
        c.Expr[B](q"")
    }
  }
  def generateMappingImpl[A: c.WeakTypeTag, B: c.WeakTypeTag]: c.Expr[Mapper[A, B]] = macroErrorGuard {
    val sourceType = weakTypeOf[A]
    val targetType = weakTypeOf[B]
//    val to = if (targetType.typeSymbol.isClass && targetType.typeSymbol.asClass.isCaseClass) {
//      q"to[${targetType}]"
//    } else {
//      q"to[$targetType]"
//    }
    //    println(q"""Mapper[$sourceType, $targetType]{ a:$sourceType => $to.by(a) }""")
    c.Expr(q"""com.teamlab.scala.konv.Mapper[$sourceType, $targetType]{
         a:$sourceType => com.teamlab.scala.konv.From(a).to[$targetType] }""")
  }
  case class SourceInfo(tree: c.Tree, tpe: c.Type, name: c.TermName)
  object SourceInfo {
    def apply(tree: c.Tree): SourceInfo = new SourceInfo(tree, tree.tpe, TermName(c.freshName()))
  }

  case class Factory(tree: Tree, method: MethodSymbol)
  def buildByConstructor[A: c.WeakTypeTag]: c.Expr[A] = macroErrorGuard {
    val typ = weakTypeOf[A]
    val constructor = typ.member(termNames.CONSTRUCTOR)
    val ftr = constructor.alternatives.filter(_.asMethod.isPublic) match {
      case head :: Nil => Factory(q"$typ", head.asMethod)
      case Nil         => c.abort(c.enclosingPosition, s"${typ} has not public constructor")
      case list =>
        list.find(_.asMethod.isPrimaryConstructor) match {
          case Some(cst) => Factory(q"$typ", cst.asMethod)
          case None =>
            c.abort(c.enclosingPosition, s"${typ} has not primary constructor. ${list.size} constructors exists")
        }
    }
    val (args, overwrites) = findArs(c.prefix.tree)
    by(ftr, args, overwrites.toMap)
  }
  def buildByFactory[A: c.WeakTypeTag](factory: c.Tree): c.Expr[A] =
    macroErrorGuard {
      val ftr = factory match {
        case q"({ (..$_) => $method(..$_) })" =>
          Factory(method, method.symbol.asMethod)
        case q"($obj) " =>
          val method = obj.tpe.member(TermName("apply"))
          Factory(obj, method.asMethod)
      }
      val (args, overwrites) = findArs(c.prefix.tree)
      by(ftr, args, overwrites.toMap)
    }
  private def findArs(
      prefix: c.Tree
  ): (Seq[c.Tree], Seq[(c.TermName, c.Tree)]) =
    prefix
      .collect {
        case q"""$from.applyDynamicNamed("apply")(..$named)""" if from.tpe =:= typeOf[From].companion =>
          val (args, overwrites) = named
            .collect {
              case q"($key, $value)" => TermName(literalString(key)) -> value
            }
            .partition(_._1.toString.isEmpty)
          args.map(_._2) -> overwrites
        case q"""$from.applyDynamic("apply")(..$args)""" if from.tpe =:= typeOf[From].companion =>
          args -> Seq()
      }
      .headOption
      .getOrElse(
        throw new MacroError(s"no match From.apply ${showRaw(c.prefix.tree)}")
      )

  private def by[B: c.WeakTypeTag](
      factory: Factory,
      args: Seq[c.Tree],
      overwrites: Map[c.TermName, c.Tree]
  ): c.Expr[B] = {
    val sources = args.map(SourceInfo.apply)
    val ret = sources.headOption
      .flatMap { s =>
        val ret = autoConvert(s.tree, s.tpe, weakTypeOf[B])
        if(ret.isDefined){
          (args.drop(1) ++ overwrites.toSeq.map(_._2)).headOption.foreach{ op =>
            throw new MacroError(s"arguments use only first", op.pos)
          }
        }
        ret
      }
      .getOrElse {
        val (pnames, params) = generateParams[B](sources, overwrites, factory.method).unzip
        val sets = pnames.flatten.toSet.map { s: SourceInfo =>
          q"""val ${s.name} = ${s.tree}"""
        }
        if (factory.tree.isType) {
          q"""{ ..$sets;  new ${factory.tree}(..$params) }"""
        } else {
          q"""{ ..$sets;  ${factory.tree}(..$params) }"""
        }
      }
//    println(ret)
    c.Expr[B](ret)
  }

  private def literalString: PartialFunction[c.Tree, String] = {
    case Literal(Constant(str: String)) => str
  }

  private def generateParams[B: WeakTypeTag](
      sources: Seq[SourceInfo],
      overwrites: Map[c.TermName, c.Tree],
      factory: MethodSymbol
  ): List[(Option[SourceInfo], c.Tree)] = {
    val params = factory.paramLists.head
    val dparams = scala.collection.mutable.Map(overwrites.toSeq: _*)
    val ret = params
      .map { parameter =>
        val name = parameter.name.toTermName
        dparams.get(name) match {
          case Some(value) =>
            dparams.remove(name)
            None -> q"$name = $value"
          case None =>
            findSourceField(sources, name) match {
              case None =>
                if (parameter.asTerm.isParamWithDefault) None -> q""
                else
                  throw new MacroError(
                    s"not enough arguments for ${factory.fullName} Unspecified value parameter $name:${parameter.typeSignature}"
                  )
              case Some((source, method)) =>
                val tep = weakTypeOf[B]
                val pt = parameter.typeSignature.asSeenFrom(tep, tep.typeSymbol.asClass)
                val sourceType = method.returnType.asSeenFrom(source.tpe, source.tpe.typeSymbol.asClass)
                Some(source) -> q"""${parameter} = ${autoConvert(q"${source.name}.$name", sourceType, pt)
                  .getOrElse(q"${source.name}.$name:$pt")}"""
            }
        }
      }
      .filterNot(_._2.isEmpty)
    if (dparams.nonEmpty) {
      val dp = dparams.map(d => s"${d._1}:${d._2.tpe}").mkString(",")
      throw new MacroError(
        s"${dp} is not in ${factory.fullName}(${params.map(p => s"${p.name}:${p.typeSignature}").mkString(", ")})",
        dparams.head._2.pos
      )
    }
    ret
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
    inferImplicitValue(sourceType, targetType)
      .map { mapper =>
        /** if exists `implicit Mapper[SOURCE, TARGET]` */
        q"$mapper.map($source)"
      }
      .orElse(
        /** if is `TARGET extends SOURCE` */
        if (sourceType <:< targetType) Some(q"$source")
        else
          findSingleValueConstructor(targetType, sourceType)
          /** if exists constructor `new TARGET(SOURCE)` */
            .map(_ => q"new $targetType($source)")
            .orElse {
              findSingleValueCaseClassParam(sourceType).flatMap { param =>
                /** if `case class SOURCE(TARGET)` then SOURCE.TARGET */
                if (param.typeSignature <:< targetType) Some(q"$source.${param.name.toTermName}")
                else
                  findSingleValueConstructor(targetType, param.typeSignature)
                  /** if `case class SOURCE(PARAM1)` and `new TARGET(SOURCE.PARAM1)` */
                    .map(_ => q"new $targetType($source.${param.name.toTermName})")
              }
            }
      )
  def inferImplicitValue(sourceType: c.Type, targetType: c.Type): Option[c.Tree] = {
    val t = c.inferImplicitValue(
      appliedType(typeOf[Mapper[_, _]], List(sourceType, targetType)),
      silent = true,
      withMacrosDisabled = false
    )
    if (t.isEmpty) None else Some(t)
  }

  def findSingleValueConstructor(t: c.Type, o: c.Type) = {
    t.typeConstructor
      .member(c.universe.termNames.CONSTRUCTOR)
      .alternatives
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
