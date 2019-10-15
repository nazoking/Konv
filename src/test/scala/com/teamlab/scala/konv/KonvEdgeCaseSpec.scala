package com.teamlab.scala.konv

import org.scalatest.DiagrammedAssertions
import org.scalatest.exceptions.TestFailedException
import org.scalatest.refspec.RefSpec

class KonvEdgeCaseSpec extends RefSpec with DiagrammedAssertions {
  def `can use reserved keyword`: Unit = {
    case class Target(`type`: String, `class`: Int)
    case class Source(`type`: String, `class`: Int)
    assert(From(Source("test", 1)).to[Target] == Target("test", 1))
  }
  object `can use constructor` {
    case class Source(primary: String = "primary", second: BigDecimal = BigDecimal(2.1), y: Int = 3)
    def `use primary constructor(1)` : Unit = {
      class Target(val primary: String) {
        def this(second: BigDecimal, y: Int) = this("second")
      }
      assert(From(Source()).to[Target].primary == "primary")
    }

    def `use primary constructor(2)` : Unit = {
      class Target(val primary: String, y: Int) {
        def this(second: BigDecimal) = this("second", 10)
      }
      assert(From(Source()).to[Target].primary == "primary")
    }

    def `when primary constructor has no params`: Unit = {
      class Target {
        var test: String = "no"
        def this(second: String, y: Int) {
          this()
          fail()
        }
      }
      assert(From(Source()).to[Target].test == "no")
    }

    def `private primary constructor and one public constructor`: Unit = {
      class Target private (val primary: String) {
        def this(second: BigDecimal, y: Int) = this("second")
      }
      assert(From(Source()).to[Target].primary == "second")
    }

    def `Can't use if private primary constructor and many public constructors`: Unit = {
      class Target private (val primary: String) {
        def this(y: Int) = this(y.toString)
        def this(second: BigDecimal) = this("second")
      }
      val src = Source()
      assert(getTestErrorMessage(assertCompiles("""
        From(src).to[Target]
      """)).contains("has not primary constructor"))
    }
  }
  object `can use factory` {
    object `can select factory overrides` {
      def factory(a: Int): Long = 1
      def factory(a: Int, b: Long): Long = 2
      case class Source(a: Int = 1, b: Long = 2)
      def `select 1`: Unit = {
        assert(From(Source()).to(factory(_: Int)) == 1)
      }
      def `select 2`: Unit = {
        assert(From(Source()).to(factory(_: Int, _: Long)) == 2)
      }
    }
  }
  object `implicit konv` {
    case class Target(x: String)
    case class Source(x: Long = 1)
    def `can compile if dose not define implicit Konv`: Unit = {
      assert(getTestErrorMessage(assertCompiles("""
      From(Source()).to[Target] == Target("1")
      """)).contains("type mismatch"))
    }
    def `can compile if defined implicit Konv`: Unit = {
      implicit val x = Konv[Long, String](x => x.toString)
      assert(From(Source()).to[Target] == Target("1"))
    }
  }
  object `warp unwrap single value class` {
    case class Target1(name: String)
    case class Target(x: Target1)
    case class Source1(x: String)
    def `auto wrap`: Unit = {
      assert(From(Source1("1")).to[Target] == Target(Target1("1")))
    }
    def `auto unwrap`: Unit = {
      assert(From(Target(Target1("1"))).to[Source1] == Source1("1"))
    }
  }
  object `generic class` {
    case class S1(a: String, v: Int)
    case class Source[A](name: String, value: A)
    def `generics`: Unit = {
      case class T1(a: String, v: Int)
      case class Target[A](name: String, value: A)
      implicit val x = Konv.mapper[S1, T1]
      assert(From(Source("x", S1("a", 1))).to[Target[T1]] == Target("x", T1("a", 1)))
    }
    def `generics 2`: Unit = {
      case class T1[A](a: A, v: Int)
      case class Target[A](name: String, value: A)
      implicit val x = Konv.mapper[S1, T1[String]]
      assert(From(Source("x", S1("a", 1))).to[Target[T1[String]]] == Target("x", T1("a", 1)))
    }
    def `generics inner class`: Unit = {
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
