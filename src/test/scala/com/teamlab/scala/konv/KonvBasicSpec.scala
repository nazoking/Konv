package com.teamlab.scala.konv

import utest._

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
    test("Apply Overrids") {
      case class Source(bbb: Int, aaa: String)

      case class Target(aaa: String, bbb: Int)
      object Target {
        // but not return type
        def apply(a: Int): Int = a

        // but not public
        private def apply(a: Long): Target = {
          println(a + 2) // dismiss warning
          ???
        }
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
      test("need constructor") {
        trait Target
        case class Source(bbb: Int, aaa: String)
        classOf[Target].getClass // dismiss warning
        assert(compileError("""
        From(Source(100, "xxx")).to[Target]
      """).msg.contains("has not public constructor"))
      }
      test("constructor has no arguments") {
        class Target
        case class Source(bbb: Int)
        assert(From(Source(100)).to[Target].isInstanceOf[Target])
      }
      test("constructor has one arguments") {
        class Target(val bbb: Int)
        case class Source(bbb: Int)
        assert(From(Source(100)).to[Target].bbb == 100)
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
