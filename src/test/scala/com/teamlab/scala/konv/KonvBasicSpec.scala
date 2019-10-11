package com.teamlab.scala.konv

import scala.language.experimental.macros

import org.scalatest.{DiagrammedAssertions, FunSpec, Matchers}

class KonvBasicSpec extends FunSpec with Matchers with DiagrammedAssertions {
  describe("test"){
    describe("Simple"){
      case class Source(bbb: Int, aaa: String)

      case class Target(aaa: String, bbb: Int)

      val src = Source(10, "hhh")
      val tar = Target("hhh", 10)
      val tar2 = tar.copy(aaa = "!"+tar.aaa)

      it("create by constructor") {
        assert(Konv.to[Target].by(src) == tar)
        assert(Konv.to[Target].by(src, aaa="!"+src.aaa) == tar2)
      }
      it("create by companion") {
        assert(Konv.to(Target).by(src) == tar)
        assert(Konv.to(Target).by(src, aaa="!" + src.aaa) == tar2)
      }
      it("create by factory") {
        assert(Konv.to(Target.apply _).by(src) == tar)
        assert(Konv.to(Target.apply _).by(src, aaa="!"+src.aaa) == tar2)
      }
    }
    it("Renamed"){
      case class Source(bbb: Int, aaa: String, ccc:Long)

      case class Target(aaa: String, bbb: Int, CCC:Long)

      val src = Source(10, "hhh", 2)
      val tar = Target("hhh", 10, 2)
      assert(Konv.to[Target].by(src, CCC=src.ccc) == tar)
    }
    it("SubConvert"){
      case class SSub(aaa: Int)
      case class TSub(aaa: Int)

      case class Source(aaa:SSub)
      case class Target(aaa:TSub)
      val src = Source(SSub(10))
      val tar = Target(TSub(10))

      implicit val sub = Konv[SSub, TSub]{ a:SSub => Konv.to[TSub].by(a) }
      assert(Konv.to(Target).by(src) == tar)
    }
    it("ApplyOverrids"){
      case class Source(bbb: Int, aaa: String)

      case class Target(aaa: String, bbb: Int)
      object Target{
        // but not return type
        def apply(a:Int):Int = a
        // but not public
        private def apply(a:Long):Target = Target("xxx", a.toInt)
      }

      val src = Source(10, "hhh")
      val tar = Target("hhh", 10)

      assert(Konv.to[Target].by(src) == tar)
    }
    describe("TraitAndCompanion"){
      case class Source(bbb: Int, aaa: String)

      trait Target{ def aaa: String;  def bbb: Int }
      object Target{
        class Impl(val bbb: Int, val aaa: String) extends Target{
          override def toString: String = s"Target.Impl($bbb, $aaa)"

          override def equals(obj: Any): Boolean = obj match {
            case x:Target => x.aaa == this.aaa && x.bbb == this.bbb
            case _ => false
          }
        }
        def apply(bbb: Int, aaa: String) = new Impl(bbb, aaa)
      }

      it("TraitAndCompanion factory") {
        assert(Konv.to(Target.apply _).by(Source(10, "hhh")) == Target(10, "hhh"))
      }
    }
    describe("create by constructor") {
      it("need constructor"){
        trait Target
        case class Source(bbb: Int, aaa: String)
        assertDoesNotCompile("""
          Konv.to[Target].by(Source(100, "xxx"))
        """)
      }
      it("constructor has no arguments"){
        class Target
        case class Source(bbb: Int)
        assert(Konv.to[Target].by(Source(100)).isInstanceOf[Target])
      }
      it("constructor has one arguments"){
        class Target(val bbb: Int)
        case class Source(bbb: Int)
        assert(Konv.to[Target].by(Source(100)).bbb == 100)
      }
    }
    describe("create by function") {
      trait Target
      class TargetImpl(val x:String, val y:Int) extends Target{
        override def equals(obj: Any): Boolean = obj.asInstanceOf[TargetImpl].x == this.x && obj.asInstanceOf[TargetImpl].y == this.y
        override def toString: String = s"TargetImpl($x, $y)"
      }
      it("test"){
        case class Source(bbb: Int, aaa: String)
        def factory(aaa:String, bbb:Int):Target = new TargetImpl(aaa, bbb)
        assert(Konv.to(factory _).by(Source(100, "xxx")) == new TargetImpl("xxx", 100))
      }
      it("use default"){
        case class Source(aaa: String)
        def factory(aaa:String, bbb:Int=100):Target = new TargetImpl(aaa, bbb)
        assert(Konv.to(factory _).by(Source("xxx")) == new TargetImpl("xxx", 100))
      }
    }
  }
}