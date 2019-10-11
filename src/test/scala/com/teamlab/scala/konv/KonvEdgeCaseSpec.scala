package com.teamlab.scala.konv

import org.scalatest.{DiagrammedAssertions, FunSpec, Matchers}

class KonvEdgeCaseSpec extends FunSpec with DiagrammedAssertions {
  describe("corner case"){
    it("can use reserved keyword"){
      case class Target(`type`:String)
      case class Source(`type`:String)
      assert(Konv.to[Target].by(Source("test")) == Target("test"))
    }
    it("use primary constructor") {
      case class Source(primary:String, second: String, y:Int)
      class Target(val primary: String) {
        def this(second: String, y:Int) {
          this(second)
        }
      }
      assert(Konv.to[Target].by(Source("primary", "second", 2)).primary == "primary")
    }
    it("no params primary constructor") {
      case class Source(primary:String, second: String, y:Int)
      class Target {
        var test: String = "no"
        def this(second: String, y:Int) {
          this()
          test = second
        }
      }
      assert(Konv.to[Target].by(Source("primary", "second", 2)).test == "no")
    }
    it("private primary constructor and one public constructor") {
      case class Source(primary:String, second: String, y:Int)
      class Target private (val primary: String) {
        def this(second: String, y:Int) {
          this(second)
        }
      }
      assert(Konv.to[Target].by(Source("primary", "second", 2)).primary == "second")
    }
    it("Can't use if private primary constructor and many public constructors") {
      case class Source(primary:String, second: BigDecimal, y:Int)
      class Target private (val primary: String) {
        def this(y:Int) {
          this(y.toString)
        }
        def this(second:BigDecimal) {
          this(second.toString)
        }
      }
      val src = Source("primary", BigDecimal("2.1"), 3)
      assertDoesNotCompile("""
        Konv.to[Target].by(src)
      """)
    }
    describe("implicit konv"){
      case class Target1(name: String)
      case class Target(x:Target1)
      case class Source(x:Long)
      val source = Source(1)
      val target = Target(Target1("1"))
      it("can compile if dose not define implicit Konv"){
        assertDoesNotCompile("""
        Konv.to[Target].by(source) == target
        """)
      }
      it("can compile if defined implicit Konv") {
        implicit val x = Konv[Long, Target1](x => Target1(x.toString))
        assert(Konv.to[Target].by(source) == target)
      }
    }
  }
  describe("warp unwarp single value class"){
    case class Target1(name: String)
    case class Target(x: Target1)
    case class Source1(x: String)
    it("auto wrap") {
      assert(Konv.to[Target].by(Source1("1")) == Target(Target1("1")))
    }
    it("auto unwrap") {
      assert(Konv.to[Source1].by(Target(Target1("1"))) == Source1("1"))
    }
  }
}
