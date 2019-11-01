package com.teamlab.scala.konv

import utest._
import com.github.ghik.silencer.silent

object KonvBasicSpec extends TestSuite {
  val tests = Tests {
    test("Simple ") {
      case class Source(bbb: Int, aaa: String)

      case class Target(aaa: String, bbb: Int)

      val src = Source(10, "hhh")
      val tar = Target("hhh", 10)
      val tar2 = tar.copy(aaa = "!" + tar.aaa)

      test("create by constructor") {
        assert(From(src).to[Target] == tar)
        assert(From(src, aaa = "!" + src.aaa).to[Target] == tar2)
      }
      test("create by companion") {
        assert(From(src).to(Target) == tar)
        assert(From(src, aaa = "!" + src.aaa).to(Target) == tar2)
      }
      test("create by factory") {
        assert(From(src).to(Target.apply _) == tar)
        assert(From(src, aaa = "!" + src.aaa).to(Target.apply _) == tar2)
      }
    }
    test("getCode") {
      implicit val v = Mapper { a: Int =>
        a.toString
      }
      From(1).to[String] ==> "1"
      compileError("""
      From(1).getCode.to[String]
      """).check("", "v.map(1)")
    }
    test("Renamed ") {
      case class Source(bbb: Int, aaa: String, ccc: Long)

      case class Target(aaa: String, bbb: Int, CCC: Long)

      val src = Source(10, "hhh", 2)
      val tar = Target("hhh", 10, 2)
      assert(From(src, CCC = src.ccc).to[Target] == tar)
    }
    test("Sub Convert") {
      case class SSub(aaa: Int)
      case class TSub(aaa: Int)

      case class Source(aaa: SSub)
      case class Target(aaa: TSub)
      val src = Source(SSub(10))
      val tar = Target(TSub(10))

      //    implicit val sub = Konv[SSub, TSub] { a: SSub =>
      //      From(a).to[TSub]
      //    }
      assert(From(src).to(Target) == tar)
    }
    test("Apply Override") {
      case class Source(bbb: Int, aaa: String)

      case class Target(aaa: String, bbb: Int)
      object Target {
        // but not return type
        def apply(a: Int): Int = a

        // but not public
        @silent private def apply(a: Long): Target = ???
      }

      val src = Source(10, "hhh")
      val tar = Target("hhh", 10)

      assert(From(src).to[Target] == tar)
    }
    test("Trait And Companion") {
      case class Source(bbb: Int, aaa: String)

      trait Target {
        def aaa: String;

        def bbb: Int
      }
      object Target {

        class Impl(val bbb: Int, val aaa: String) extends Target

        def apply(bbb: Int, aaa: String) = new Impl(bbb, aaa)
      }

      test("TraitAndCompanion factory") {
        val ac = From(Source(10, "hhh")).to(Target.apply _)
        assert(ac.bbb == 10)
        assert(ac.aaa == "hhh")
      }
    }
    test("create by constructor") {
      case class Source(bbb: Int, aaa: String)
      val source = Source(100, "test")
      test("need constructor") {
        trait Target
        classOf[Target].getClass // dismiss warning
        assert(compileError("""
        From(source).to[Target]
      """).msg.contains("has not public constructor"))
      }
      test("constructor has no arguments") {
        class Target
        assert(From(source).to[Target].isInstanceOf[Target])
      }
      test("constructor has one arguments") {
        class Target(val bbb: Int)
        assert(From(source).to[Target].bbb == 100)
      }
      test("constructor has one arguments but field unmatched") {
        @silent class Target(val xxx: Int)
        compileError(""" From(source).to[Target] """).check("", "unspecified value parameter xxx")
      }
      test("constructor has source type one argument") {
        class Target(val xxx: Source)
        From(source).to[Target].xxx ==> source
      }
      test("constructor has source type two argument") {
        @silent class Target(val xxx: Source, val yyy: Source)
        compileError(""" From(source).to[Target] """).check("", "unspecified value parameter")
      }
      test("private constructor") {
        @silent class Target private (val xxx: Source)
        compileError(""" From(source).to[Target] """).check("", "has not public constructor")
      }
      test("private primary and sub public constructor") {
        class Target private (val xxx: Source) {
          def this(aaa: String) {
            this(Source(999, "x" + aaa))
          }
        }
        From(source).to[Target].xxx.aaa ==> "xtest"
      }
      test("private primary and sub multi public constructor") {
        @silent class Target private (val xxx: Source) {
          def this(aaa: String) {
            this(source)
          }
          def this(bbb: Int) {
            this(source)
          }
        }
        compileError("""
          From(source).to[Target]
        """).check("", "Target has not primary constructor. 2 constructors exists")
      }
    }
    test("create by function") {
      object createByFunction {
        trait Target
        class TargetImpl(val x: String, val y: Int) extends Target {
          override def equals(obj: Any): Boolean =
            obj.asInstanceOf[TargetImpl].x == this.x && obj.asInstanceOf[TargetImpl].y == this.y
          override def toString: String = s"TargetImpl($x, $y)"
        }

      }
      test("test ") {
        object Test {
          case class Source(bbb: Int, aaa: String)
          def factory(aaa: String, bbb: Int): createByFunction.Target = new createByFunction.TargetImpl(aaa, bbb)
        }

        From(Test.Source(100, "xxx")).to(Test.factory _) ==> new createByFunction.TargetImpl("xxx", 100)
      }
      test("use default") {
        object Test2 {
          case class Source(aaa: String)
          def factory(aaa: String, bbb: Int = 100): createByFunction.Target = new createByFunction.TargetImpl(aaa, bbb)
        }
        From(Test2.Source("xxx")).to(Test2.factory _) ==> new createByFunction.TargetImpl("xxx", 100)
      }
    }
  }
}
