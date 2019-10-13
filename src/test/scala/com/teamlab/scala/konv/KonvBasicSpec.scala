package com.teamlab.scala.konv

import org.scalatest.exceptions.TestFailedException

import scala.language.experimental.macros
import org.scalatest.{DiagrammedAssertions, FunSpec, Matchers}

class KonvBasicSpec extends FunSpec with Matchers with DiagrammedAssertions {
  describe("test") {
    describe("Simple") {
      case class Source(bbb: Int, aaa: String)

      case class Target(aaa: String, bbb: Int)

      val src = Source(10, "hhh")
      val tar = Target("hhh", 10)
      val tar2 = tar.copy(aaa = "!" + tar.aaa)

      it("create by constructor") {
        assert(From(src).to[Target] == tar)
        assert(From(src, aaa = "!" + src.aaa).to[Target] == tar2)
      }
      it("create by companion") {
        assert(From(src).to(Target) == tar)
        assert(From(src, aaa = "!" + src.aaa).to(Target) == tar2)
      }
      it("create by factory") {
        assert(From(src).to(Target.apply _) == tar)
        assert(From(src, aaa = "!" + src.aaa).to(Target.apply _) == tar2)
      }
    }
    it("Renamed") {
      case class Source(bbb: Int, aaa: String, ccc: Long)

      case class Target(aaa: String, bbb: Int, CCC: Long)

      val src = Source(10, "hhh", 2)
      val tar = Target("hhh", 10, 2)
      assert(From(src, CCC = src.ccc).to[Target] == tar)
    }
    it("SubConvert") {
      case class SSub(aaa: Int)
      case class TSub(aaa: Int)

      case class Source(aaa: SSub)
      case class Target(aaa: TSub)
      val src = Source(SSub(10))
      val tar = Target(TSub(10))

      implicit val sub = Konv[SSub, TSub] { a: SSub =>
        From(a).to[TSub]
      }
      assert(From(src).to(Target) == tar)
    }
    it("ApplyOverrids") {
      case class Source(bbb: Int, aaa: String)

      case class Target(aaa: String, bbb: Int)
      object Target {
        // but not return type
        def apply(a: Int): Int = a
        // but not public
        private def apply(a: Long): Target = Target("xxx", a.toInt)
      }

      val src = Source(10, "hhh")
      val tar = Target("hhh", 10)

      assert(From(src).to[Target] == tar)
    }
    describe("TraitAndCompanion") {
      case class Source(bbb: Int, aaa: String)

      trait Target { def aaa: String; def bbb: Int }
      object Target {
        class Impl(val bbb: Int, val aaa: String) extends Target {
          override def toString: String = s"Target.Impl($bbb, $aaa)"

          override def equals(obj: Any): Boolean = obj match {
            case x: Target => x.aaa == this.aaa && x.bbb == this.bbb
            case _         => false
          }
        }
        def apply(bbb: Int, aaa: String) = new Impl(bbb, aaa)
      }

      it("TraitAndCompanion factory") {
        assert(From(Source(10, "hhh")).to(Target.apply _) == Target(10, "hhh"))
      }
    }
    describe("create by constructor") {
      it("need constructor") {
        trait Target
        case class Source(bbb: Int, aaa: String)
        assert(intercept[TestFailedException](assertCompiles("""
          From(Source(100, "xxx")).to[Target]
        """)).getMessage().contains("has not public constructor"))
      }
      it("constructor has no arguments") {
        class Target
        case class Source(bbb: Int)
        assert(From(Source(100)).to[Target].isInstanceOf[Target])
      }
      it("constructor has one arguments") {
        class Target(val bbb: Int)
        case class Source(bbb: Int)
        assert(From(Source(100)).to[Target].bbb == 100)
      }
    }
    describe("create by function") {
      trait Target
      class TargetImpl(val x: String, val y: Int) extends Target {
        override def equals(obj: Any): Boolean =
          obj.asInstanceOf[TargetImpl].x == this.x && obj.asInstanceOf[TargetImpl].y == this.y
        override def toString: String = s"TargetImpl($x, $y)"
      }
      it("test") {
        case class Source(bbb: Int, aaa: String)
        def factory(aaa: String, bbb: Int): Target = new TargetImpl(aaa, bbb)
        assert(From(Source(100, "xxx")).to(factory _) == new TargetImpl("xxx", 100))
      }
      it("use default") {
        case class Source(aaa: String)
        def factory(aaa: String, bbb: Int = 100): Target = new TargetImpl(aaa, bbb)
        assert(From(Source("xxx")).to(factory _) == new TargetImpl("xxx", 100))
      }
    }
  }
}
