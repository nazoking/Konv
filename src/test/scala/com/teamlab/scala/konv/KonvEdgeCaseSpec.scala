package com.teamlab.scala.konv

import org.scalatest.exceptions.TestFailedException
import org.scalatest.{DiagrammedAssertions, FunSpec, Matchers}

class KonvEdgeCaseSpec extends FunSpec with DiagrammedAssertions {
  describe("corner case") {
    it("can use reserved keyword") {
      case class Target(`type`: String)
      case class Source(`type`: String)
      assert(From(Source("test")).to[Target] == Target("test"))
    }
    it("use primary constructor") {
      case class Source(primary: String, second: String, y: Int)
      class Target(val primary: String) {
        def this(second: String, y: Int) {
          this(second)
        }
      }
      assert(From(Source("primary", "second", 2)).to[Target].primary == "primary")
    }
    it("no params primary constructor") {
      case class Source(primary: String, second: String, y: Int)
      class Target {
        var test: String = "no"
        def this(second: String, y: Int) {
          this()
          test = second
        }
      }
      assert(From(Source("primary", "second", 2)).to[Target].test == "no")
    }
    it("private primary constructor and one public constructor") {
      case class Source(primary: String, second: String, y: Int)
      class Target private (val primary: String) {
        def this(second: String, y: Int) {
          this(second)
        }
      }
      assert(From(Source("primary", "second", 2)).to[Target].primary == "second")
    }
    it("Can't use if private primary constructor and many public constructors") {
      case class Source(primary: String, second: BigDecimal, y: Int)
      class Target private (val primary: String) {
        def this(y: Int) {
          this(y.toString)
        }
        def this(second: BigDecimal) {
          this(second.toString)
        }
      }
      val src = Source("primary", BigDecimal("2.1"), 3)
      assert(getTestErrorMessage(assertCompiles("""
        From(src).to[Target]
      """)).contains("has not primary constructor"))
    }
    describe("implicit konv") {
      case class Target1(name: String)
      case class Target(x: Target1)
      case class Source(x: Long)
      val source = Source(1)
      val target = Target(Target1("1"))
      it("can compile if dose not define implicit Konv") {
        assert(getTestErrorMessage(assertCompiles("""
        From(source).to[Target] == target
        """)).contains("type mismatch"))
      }
      it("can compile if defined implicit Konv") {
        implicit val x = Konv[Long, Target1](x => Target1(x.toString))
        assert(From(source).to[Target] == target)
      }
    }
  }
  describe("warp unwarp single value class") {
    case class Target1(name: String)
    case class Target(x: Target1)
    case class Source1(x: String)
    it("auto wrap") {
      assert(From(Source1("1")).to[Target] == Target(Target1("1")))
    }
    it("auto unwrap") {
      assert(From(Target(Target1("1"))).to[Source1] == Source1("1"))
    }
  }
  describe("generic class") {
    case class S1(a: String, v: Int)
    case class Source[A](name: String, value: A)
    it("generics") {
      case class T1(a: String, v: Int)
      case class Target[A](name: String, value: A)
      implicit val x = Konv.mapper[S1, T1]
      assert(From(Source("x", S1("a", 1))).to[Target[T1]] == Target("x", T1("a", 1)))
    }
    it("generics 2") {
      case class T1[A](a: A, v: Int)
      case class Target[A](name: String, value: A)
      implicit val x = Konv.mapper[S1, T1[String]]
      assert(From(Source("x", S1("a", 1))).to[Target[T1[String]]] == Target("x", T1("a", 1)))
    }
    it("generics inner class") {
      case class Source[A](name: String, value: A) {
        def auto = From(this).to[Target]
        case class Target(name: String, value: A)
        def manual = Target(this.name, this.value)
      }
      val s = Source[Int]("x", 1)
      assert(s.auto == s.manual)
    }
  }
  def getTestErrorMessage(f: => Unit): String =
    (try (Left(f))
    catch {
      case e: TestFailedException => Right(e.getLocalizedMessage)
    }).right.getOrElse(fail("Expected compiler error, but not"))
}
