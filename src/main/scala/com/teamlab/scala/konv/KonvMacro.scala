package com.teamlab.scala.konv

import scala.reflect.api.Position
import scala.reflect.macros.blackbox.Context

object KonvMacro {
  private class MacroError(message: String, var pos:Option[Position] = None) extends Exception(message){
    def this(message:String, pos:Position) = this(message, Some(pos))
  }

  def builldWithParamsImpl[B: c.WeakTypeTag](c: Context)(name: c.Expr[String])(args: c.Expr[(String, Any)]*): c.Expr[B] = macroErrorGuard(c) {
    literalString(c)(name.tree) match {
      case "by" =>
        val (noKeys, keyNames) = argumentsToMap(c)(args)
        by(c)(noKeys, keyNames)
      case methodName =>
        throw new MacroError(s"method not defined $methodName", name.tree.pos)
    }
  }

  def builldWithoutParamsImpl[B: c.WeakTypeTag](c: Context)(name: c.Expr[String])(args: c.Expr[Any]): c.Expr[B] = macroErrorGuard(c) {
    literalString(c)(name.tree) match {
      case "by" =>
        by(c)(Seq(args.tree), Map.empty)
      case methodName =>
        throw new MacroError(s"method not defined $methodName", name.tree.pos)
    }
  }

  def generateMappingImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](c:Context): c.Expr[Konv[A, B]] = macroErrorGuard(c) {
    import c.universe._

    val sourceType = weakTypeOf[A]
    val targetType = weakTypeOf[B]
    val to = if (targetType.typeSymbol.isClass && targetType.typeSymbol.asClass.isCaseClass) {
      q"com.teamlab.scala.konv.Konv.to[${targetType}]"
    } else {
      q"com.teamlab.scala.konv.Konv.to[$targetType]"
    }
    //    println(q"""Konv[$sourceType, $targetType]{ a:$sourceType => $to.by(a) }""")
    c.Expr(q"""com.teamlab.scala.konv.Konv[$sourceType, $targetType]{ a:$sourceType => $to.by(a) }""")
  }

  private def macroErrorGuard[B](c:Context)(f: => c.Expr[B]):c.Expr[B] = {
    try f
    catch{
      case e:MacroError =>
        c.error(e.pos.getOrElse(c.macroApplication.pos).asInstanceOf[c.Position], e.getMessage)
        import c.universe._
        c.Expr[B](q"")
    }
  }

  def by[B: c.WeakTypeTag](c: Context)(args:Seq[c.Tree], overwrites: Map[c.TermName, c.Tree]): c.Expr[B] ={
    import c.universe._
    val sources = args.map(a => freshName(c) -> a)
    val (methodcall:Tree, method) = getFactory(c)
    val params = generateParams(c)(sources, overwrites, method)
    val sets = sources.map(s => q"""val ${s._1} = ${s._2}""")
    val ret = if(methodcall.isType){
      q"""{ ..$sets;  new $methodcall(..$params) }"""
    }else{
      q"""{ ..$sets;  $methodcall(..$params) }"""
    }
//    println(ret)
    c.Expr[B](ret)
  }

  def getFactory(c:Context): (c.Tree, c.Symbol) ={
    import c.universe._
    c.prefix.tree match {
      case q"$_.to[$typ]" =>
        val constractor = typ.tpe.member(termNames.CONSTRUCTOR)
        constractor.alternatives.filter(_.asMethod.isPublic) match {
          case head :: Nil => typ -> head
          case Nil => throw new MacroError(s"${typ.tpe} has not public constructor", typ.pos)
          case list => list.find(_.asMethod.isPrimaryConstructor) match {
            case Some(cst) => typ -> cst.asMethod
            case None => throw new MacroError(s"${typ.tpe} has not primary constructor. ${list.size} constructors exists", typ.pos)
          }
        }
      case q"$_.to[$typ]({ (..$_) => $method(..$_) })" =>
        method -> method.symbol
      case q"$_.to[$typ]($obj)" =>
        val method = obj.tpe.member(TermName("apply"))
        obj -> method
      case _ =>
        throw new MacroError(s"Can't find target (Konv.to) define", c.prefix.tree.pos)
    }
  }



  private def freshName(c:Context): c.TermName = c.universe.TermName(c.freshName())

  private def literalString(c:Context):PartialFunction[c.Tree, String] = {
    import c.universe._
    { case Literal(Constant(str:String)) => str }
  }


  private def argumentsToMap(c:Context)(args: Seq[c.Expr[(String, Any)]]): (Seq[c.Tree], Map[c.universe.TermName, c.Tree]) = {
    import c.universe._
    val keyValues = args.map(_.tree match { case q"""($key, $impl)""" => TermName(literalString(c)(key)) -> impl })
    val (noKey, map) = keyValues.partition(_._1.toString.isEmpty)
    noKey.map(_._2) -> map.toMap
  }

  private def generateParams(c:Context)(sources: Seq[(c.TermName, c.Tree)], overwrites: Map[c.TermName, c.Tree], method:c.Symbol): List[c.Tree] = {
    import c.universe._
    val params = method.asMethod.paramLists.head
    val dparams = scala.collection.mutable.Map(overwrites.toSeq: _*)
    def find(name:TermName): Option[(c.TermName, c.universe.Symbol, c.Type)] = (for {
      (src, typ) <- sources
      mem = typ.tpe.member(name) if mem != NoSymbol
      alts <- mem.alternatives if alts.isMethod
      method = alts.asMethod if method.paramLists.isEmpty || (method.paramLists.size == 1 && method.paramLists.head.isEmpty)
    } yield {
      (src , method, method.returnType)
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
              q"""${p} = ${autoConvert(c)(q"$source.$name", sourceType, p.typeSignature).getOrElse(q"$source:${p.typeSignature}")}"""
          }
      }
    }.filterNot(_.isEmpty)
    if(dparams.nonEmpty) throw new MacroError(s"${dparams.map(_._1)} is not in ${method.fullName}", dparams.head._2.pos)
    ret
  }
  def autoConvert(c:Context)(source:c.Tree, sourceType:c.Type, targetType:c.Type):Option[c.Tree] ={
    import c.universe._
    if (sourceType <:< targetType) Some(q"$source")
    else {
      // get implicit Konv[sourceType, taretType]
      val konv = c.inferImplicitValue(appliedType(typeOf[Konv[_, _]], List(sourceType, targetType)), silent = true)
      if(konv.nonEmpty) Some(q"$konv.map($source)")
      else {
        getSingleValueConstractor(c)(targetType, sourceType) match {
          case Some(_) => Some(q"new $targetType($source)")
          case None => getSingleValueCaseClassParam(c)(sourceType) match {
              case Some(param) if param.typeSignature <:< targetType => Some(q"$source.${param.name.toTermName}")
              case Some(param) => getSingleValueConstractor(c)(targetType, param.typeSignature) match {
                  case Some(_) =>Some(q"new $targetType($source.${param.name.toTermName})")
                  case None => None
                }
              case None => None
            }
        }
      }
    }
  }
  def getSingleValueConstractor(c:Context)(t:c.Type, o:c.Type) = {
    t.typeConstructor.member(c.universe.termNames.CONSTRUCTOR).alternatives.find(_.asMethod.paramLists match {
      case List(List(p)) => p.typeSignature <:< o
      case _ => false
    })
  }
  def getSingleValueCaseClassParam(c:Context)(t:c.Type) = {
    import c.universe._
    if(t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass){
      t.typeConstructor.decls.collectFirst{
        case m: MethodSymbol if m.isPrimaryConstructor => m
      }.map(_.paramLists.head).filter(_.size == 1) match {
        case Some(p :: Nil) => Some(p)
        case _ => None
      }
    }else{
      None
    }
  }
}
