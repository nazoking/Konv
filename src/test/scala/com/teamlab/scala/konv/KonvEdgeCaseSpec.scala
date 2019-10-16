package com.teamlab.scala.konv

import scala.language.implicitConversions
import org.scalatest.DiagrammedAssertions
import org.scalatest.exceptions.TestFailedException
import org.scalatest.refspec.RefSpec

class KonvEdgeCaseSpec extends RefSpec with DiagrammedAssertions {
  def `can use reserved keyword`(): Unit = {
    case class Target(`type`: String, `class`: Int)
    case class Source(`type`: String, `class`: Int)
    assert(From(Source("test", 1)).to[Target] == Target("test", 1))
  }
  object `can use constructor` {
    case class Source(primary: String = "primary", second: BigDecimal = BigDecimal(2.1), y: Int = 3)
    def `use primary constructor(1)`(): Unit = {
      class Target(val primary: String) {
        def this(second: BigDecimal, y: Int) = this("second")
      }
      assert(From(Source()).to[Target].primary == "primary")
    }

    def `use primary constructor(2)`(): Unit = {
      class Target(val primary: String, val y: Int) {
        def this(second: BigDecimal) = this("second", 10)
      }
      assert(From(Source()).to[Target].primary == "primary")
    }

    def `when primary constructor has no params`(): Unit = {
      class Target {
        var test: String = "no"
        def this(second: String, y: Int) {
          this()
          this.test = "1"
        }
      }
      assert(From(Source()).to[Target].test == "no")
    }

    def `private primary constructor and one public constructor`(): Unit = {
      class Target private (val primary: String) {
        def this(second: BigDecimal, y: Int) = this("second")
      }
      assert(From(Source()).to[Target].primary == "second")
    }

    def `Can't use if private primary constructor and many public constructors`(): Unit = {
      class Target private (val primary: String) {
        def this(y: Int) = this(y.toString)
        def this(second: BigDecimal) = this("second")
      }
      new Target(1) // dismiss warning
      assert(getTestErrorMessage(assertCompiles("""
        From(Source()).to[Target]
      """)).contains("has not primary constructor"))
    }
  }
  object `can use factory` {
    object `can select factory overrides` {
      def factory(a: Int): Long = a.toLong
      def factory(a: Int, b: Long): Long = a + b
      case class Source(a: Int = 1, b: Long = 2)
      def `select 1`(): Unit = {
        assert(From(Source()).to(factory(_: Int)) == 1)
      }
      def `select 2`(): Unit = {
        assert(From(Source()).to(factory(_: Int, _: Long)) == 3)
      }
    }
  }
  object `implicit konv` {
    case class Target(x: String)
    case class Source(x: Long = 1)
    def `can compile if dose not define implicit Konv`(): Unit = {
      assert(getTestErrorMessage(assertCompiles("""
      From(Source()).to[Target] == Target("1")
      """)).contains("type mismatch"))
    }
    def `can compile if defined implicit Konv`(): Unit = {
      implicit val x = Konv[Long, String](x => s"implicit parameter $x")
      assert(From(Source()).to[Target] == Target("implicit parameter 1"))
    }
    def `can compile if defined implicit conversion`(): Unit = {
      implicit def x(x: Long) = s"implicit conversion $x"
      assert(From(Source()).to[Target] == Target("implicit conversion 1"))
    }
    def `use implicit parameter over implicit conversion`(): Unit = {
      implicit def ic(x: Long) = s"conversion $x"
      implicit val ip = Konv[Long, String](_ => "parameter")
      val x = ic(1L)
      assert(x == "conversion 1")
      assert(From(Source()).to[Target] == Target("parameter"))
    }
  }
  object `warp unwrap single value class` {
    case class Target1(name: String)
    case class Target(x: Target1)
    case class Source1(x: String)
    def `auto wrap`(): Unit = {
      assert(From(Source1("1")).to[Target] == Target(Target1("1")))
    }
    def `auto unwrap`(): Unit = {
      assert(From(Target(Target1("1"))).to[Source1] == Source1("1"))
    }
  }
  object `side effect` {
    def `test side effect`(): Unit = {
      case class Target(a: Int)
      var x = 10
      def i = {
        x += 1
        x
      }
      assert(From(i /* no use */, a = i).to[Target] == Target(11))
      assert(i == 12)
    }
    def `test underscore`(): Unit = {
      case class Target(a: Int)
      def test : Target => Target = From(_).to[Target]
      assert(test(Target(10)) == Target(10))
    }
  }
  object `generic class` {
    case class S1(a: String, v: Int)
    case class Source[A](name: String, value: A)
    def `test generics`(): Unit = {
      case class T1(a: String, v: Int)
      case class Target[A](name: String, value: A)
      implicit val x = Konv.mapper[S1, T1]
      assert(From(Source("x", S1("a", 1))).to[Target[T1]] == Target("x", T1("a", 1)))
    }
    def `generics 2`(): Unit = {
      case class T1[A](a: A, v: Int)
      case class Target[A](name: String, value: A)
      implicit val x = Konv.mapper[S1, T1[String]]
      assert(From(Source("x", S1("a", 1))).to[Target[T1[String]]] == Target("x", T1("a", 1)))
    }
    def `generics inner class`(): Unit = {
      case class Source[A](name: String, value: A) {
        case class Target(name: String, value: A)
        def auto = From(this).to[Target]
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
