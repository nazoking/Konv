package com.teamlab.scala.konv.internal

import com.teamlab.scala.konv.{From, Konv}

import scala.reflect.api.{Trees, Position => Pos}
import scala.reflect.macros.blackbox.Context

class Macro(val c:Context) {
  import c.universe._
  private class MacroError(message: String, var pos:Option[Pos] = None) extends Exception(message){
    def this(message:String, pos:Pos) = this(message, Some(pos))
  }
  private def macroErrorGuard[B](f: => c.Expr[B]):c.Expr[B] = {
    try f
    catch{
      case e:MacroError =>
        c.error(e.pos.getOrElse(c.macroApplication.pos).asInstanceOf[c.Position], e.getMessage)
        c.Expr[B](q"")
    }
  }
  def generateMappingImpl[A: c.WeakTypeTag, B: c.WeakTypeTag]: c.Expr[Konv[A, B]] = macroErrorGuard {
    val sourceType = weakTypeOf[A]
    val targetType = weakTypeOf[B]
//    val to = if (targetType.typeSymbol.isClass && targetType.typeSymbol.asClass.isCaseClass) {
//      q"to[${targetType}]"
//    } else {
//      q"to[$targetType]"
//    }
    //    println(q"""Konv[$sourceType, $targetType]{ a:$sourceType => $to.by(a) }""")
    c.Expr(
      q"""com.teamlab.scala.konv.Konv[$sourceType, $targetType]{
         a:$sourceType => com.teamlab.scala.konv.From(a).to[$targetType] }""")
  }

  case class Factory(tree:c.Tree, method:MethodSymbol)
  def buildByConstructor[A: c.WeakTypeTag]:c.Expr[A] = macroErrorGuard{
    val typ = weakTypeOf[A]
    val constructor = typ.member(termNames.CONSTRUCTOR)
    val ftr = constructor.alternatives.filter(_.asMethod.isPublic) match {
      case head :: Nil => Factory(q"$typ", head.asMethod)
      case Nil => throw new MacroError(s"${typ} has not public constructor")
      case list => list.find(_.asMethod.isPrimaryConstructor) match {
        case Some(cst) => Factory(q"$typ" , cst.asMethod)
        case None => throw new MacroError(s"${typ} has not primary constructor. ${list.size} constructors exists")
      }
    }
    val (args, overwrites) = findArs(c.prefix.tree)
    by(ftr, args, overwrites.toMap)
  }
  def buildByFactory[A: c.WeakTypeTag](factory: c.Tree):c.Expr[A] = macroErrorGuard{
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
  private def findArs(prefix: c.Tree): (Seq[c.Tree], Seq[(c.TermName, c.Tree)]) = prefix.collect{
      case q"""$from.applyDynamicNamed("apply")(..$named)""" if from.tpe =:= typeOf[From].companion =>
        val (args, overwrites) = named.collect{ case q"($key, $value)" => TermName(literalString(key)) -> value }
          .partition(_._1.toString.isEmpty)
        args.map(_._2) -> overwrites
      case q"""$from.applyDynamic("apply")(..$args)""" if from.tpe =:= typeOf[From].companion =>
        args -> Seq()
    }.headOption.getOrElse(throw new MacroError(s"no match From.apply ${showRaw(c.prefix.tree)}"))

  private def by[B: c.WeakTypeTag](factory:Factory, args:Seq[c.Tree], overwrites: Map[c.TermName, c.Tree]): c.Expr[B] ={
    val sources = args.map(a => TermName(c.freshName()) -> a)
    val params = generateParams[B](sources, overwrites, factory.method)
    val sets = sources.map(s => q"""val ${s._1} = ${s._2}""")
    val ret = if(factory.tree.isType){
      q"""{ ..$sets;  new ${factory.tree}(..$params) }"""
    }else{
      q"""{ ..$sets;  ${factory.tree}(..$params) }"""
    }
//    println(ret)
    c.Expr[B](ret)
  }

  private def literalString:PartialFunction[c.Tree, String] = {
    case Literal(Constant(str:String)) => str
  }

  private def generateParams[B: WeakTypeTag](sources: Seq[(c.TermName, c.Tree)], overwrites: Map[c.TermName, c.Tree], method:c.Symbol): List[c.Tree] = {
    val params = method.asMethod.paramLists.head
    val dparams = scala.collection.mutable.Map(overwrites.toSeq: _*)
    def find(name:TermName): Option[(c.TermName, c.universe.Symbol, c.Type)] = (for {
      (src, typ) <- sources
      mem = typ.tpe.member(name) if mem != NoSymbol
      alts <- mem.alternatives if alts.isMethod
      m2 = alts.asMethod if m2.paramLists.isEmpty || (m2.paramLists.size == 1 && m2.paramLists.head.isEmpty)
    } yield {
      (src , m2, m2.returnType.asSeenFrom(typ.tpe, typ.tpe.typeSymbol.asClass))
    }).headOption
    val ret = params.map { p =>
      val name = p.name.toTermName
      dparams.get(name) match {
        case Some(value) =>
          dparams.remove(name)
          q"$name = $value"
        case None =>
          find(name) match {
            case None =>
              if (p.asTerm.isParamWithDefault) q""
              else throw new MacroError(s"not enough arguments for ${method.fullName} Unspecified value parameter $name:${p.typeSignature}")
            case Some((source, m, sourceType)) =>
              val tep = weakTypeOf[B]
              val pt = p.typeSignature.asSeenFrom(tep, tep.typeSymbol.asClass)
              q"""${p} = ${autoConvert(q"$source.$name", sourceType, pt).getOrElse(q"$source:${pt}")}"""
          }
      }
    }.filterNot(_.isEmpty)
    if(dparams.nonEmpty) throw new MacroError(s"${dparams.keys} is not in ${method.fullName}", dparams.head._2.pos)
    ret
  }
  def autoConvert(source:c.Tree, sourceType:c.Type, targetType:c.Type):Option[c.Tree] ={
    if (sourceType <:< targetType) Some(q"$source")
    else {
      // get implicit Konv[sourceType, taretType]
      val konv = c.inferImplicitValue(appliedType(typeOf[Konv[_, _]], List(sourceType, targetType)), silent = true)
      if(konv.nonEmpty) Some(q"$konv.map($source)")
      else {
        getSingleValueConstructor(targetType, sourceType) match {
          case Some(_) => Some(q"new $targetType($source)")
          case None => getSingleValueCaseClassParam(sourceType) match {
            case Some(param) if param.typeSignature <:< targetType => Some(q"$source.${param.name.toTermName}")
            case Some(param) => getSingleValueConstructor(targetType, param.typeSignature) match {
              case Some(_) =>Some(q"new $targetType($source.${param.name.toTermName})")
              case None => None
            }
            case None => None
          }
        }
      }
    }
  }
  def getSingleValueConstructor(t:c.Type, o:c.Type) = {
    t.typeConstructor.member(c.universe.termNames.CONSTRUCTOR).alternatives.find(_.asMethod.paramLists match {
      case List(List(p)) => p.typeSignature <:< o
      case _ => false
    })
  }
  def getSingleValueCaseClassParam(t:c.Type) = {
    if(t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass){
      t.typeConstructor.decls.collectFirst{
        case m: MethodSymbol if m.isPrimaryConstructor => m
      }.map(_.paramLists.head) match {
        case Some(p :: Nil) => Some(p)
        case _ => None
      }
    }else{
      None
    }
  }
}
