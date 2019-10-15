package com.teamlab.scala.konv

import org.scalatest.exceptions.TestFailedException
import scala.language.experimental.macros

import org.scalatest.refspec.RefSpec
import org.scalatest.{DiagrammedAssertions, FunSpec, Matchers}

class KonvBasicSpec extends RefSpec with Matchers with DiagrammedAssertions {
  object `Simple` {
    case class Source(bbb: Int, aaa: String)

    case class Target(aaa: String, bbb: Int)

    val src = Source(10, "hhh")
    val tar = Target("hhh", 10)
    val tar2 = tar.copy(aaa = "!" + tar.aaa)

    def `create by constructor`: Unit = {
      assert(From(src).to[Target] == tar)
      assert(From(src, aaa = "!" + src.aaa).to[Target] == tar2)
    }
    def `create by companion`: Unit = {
      assert(From(src).to(Target) == tar)
      assert(From(src, aaa = "!" + src.aaa).to(Target) == tar2)
    }
    def `create by factory`: Unit = {
      assert(From(src).to(Target.apply _) == tar)
      assert(From(src, aaa = "!" + src.aaa).to(Target.apply _) == tar2)
    }
  }
  def `Renamed`: Unit = {
    case class Source(bbb: Int, aaa: String, ccc: Long)

    case class Target(aaa: String, bbb: Int, CCC: Long)

    val src = Source(10, "hhh", 2)
    val tar = Target("hhh", 10, 2)
    assert(From(src, CCC = src.ccc).to[Target] == tar)
  }
  def `SubConvert`: Unit = {
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
  def `ApplyOverrids`: Unit = {
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
  object `TraitAndCompanion` {
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

    def `TraitAndCompanion factory`: Unit = {
      assert(From(Source(10, "hhh")).to(Target.apply _) == Target(10, "hhh"))
    }
  }
  object `create by constructor` {
    def `need constructor`: Unit = {
      trait Target
      case class Source(bbb: Int, aaa: String)
      assert(intercept[TestFailedException](assertCompiles("""
        From(Source(100, "xxx")).to[Target]
      """)).getMessage().contains("has not public constructor"))
    }
    def `constructor has no arguments`: Unit = {
      class Target
      case class Source(bbb: Int)
      assert(From(Source(100)).to[Target].isInstanceOf[Target])
    }
    def `constructor has one arguments`: Unit = {
      class Target(val bbb: Int)
      case class Source(bbb: Int)
      assert(From(Source(100)).to[Target].bbb == 100)
    }
  }
  object `create by function` {
    trait Target
    class TargetImpl(val x: String, val y: Int) extends Target {
      override def equals(obj: Any): Boolean =
        obj.asInstanceOf[TargetImpl].x == this.x && obj.asInstanceOf[TargetImpl].y == this.y
      override def toString: String = s"TargetImpl($x, $y)"
    }
    def `test`: Unit = {
      case class Source(bbb: Int, aaa: String)
      def factory(aaa: String, bbb: Int): Target = new TargetImpl(aaa, bbb)
      assert(From(Source(100, "xxx")).to(factory _) == new TargetImpl("xxx", 100))
    }
    def `use default`: Unit = {
      case class Source(aaa: String)
      def factory(aaa: String, bbb: Int = 100): Target = new TargetImpl(aaa, bbb)
      assert(From(Source("xxx")).to(factory _) == new TargetImpl("xxx", 100))
    }
  }
}
