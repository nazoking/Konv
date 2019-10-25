package com.teamlab.scala.konv

import scala.language.implicitConversions
import utest._

object KonvEdgeCaseSpec extends TestSuite {
  def tests = Tests {
    test("can use reserved keyword") {
      case class Target(`type`: String, `class`: Int)
      case class Source(`type`: String, `class`: Int)
      assert(From(Source("test", 1)).to[Target] == Target("test", 1))
    }
    test("can use constructor") {
      case class Source(primary: String = "primary", second: BigDecimal = BigDecimal(2.1), y: Int = 3)
      test("use primary constructor(1)") {
        class Target(val primary: String) {
          def this(second: BigDecimal, y: Int) = this("second")
        }
        assert(From(Source()).to[Target].primary == "primary")
      }

      test("use primary constructor(2)") {
        class Target(val primary: String, val y: Int) {
          def this(second: BigDecimal) = this("second", 10)
        }
        assert(From(Source()).to[Target].primary == "primary")
      }

      test("when primary constructor has no params") {
        class Target {
          var test: String = "no"

          def this(second: String, y: Int) {
            this()
            this.test = "1"
          }
        }
        assert(From(Source()).to[Target].test == "no")
      }

      test("private primary constructor and one public constructor") {
        class Target private (val primary: String) {
          def this(second: BigDecimal, y: Int) = this("second")
        }
        assert(From(Source()).to[Target].primary == "second")
      }

      test("Can't use if private primary constructor and many public constructors") {
        class Target private (val primary: String) {
          def this(y: Int) = this(y.toString)

          def this(second: BigDecimal) = this("second")
        }
        new Target(1) // dismiss warning
        assert(compileError("""
        From(Source()).to[Target]
      """).msg.contains("has not primary constructor"))
      }
    }
    test("can use factory") {
      test("can select factory overrides") {
        object Test {
          def factory(a: Int): Long = a.toLong

          def factory(a: Int, b: Long): Long = a + b
        }
        case class Source(a: Int = 1, b: Long = 2)
        test("select 1") {
          assert(From(Source()).to(Test.factory(_: Int)) == 1)
        }
        test("select 2") {
          assert(From(Source()).to(Test.factory(_: Int, _: Long)) == 3)
        }
      }
    }
    test("implicit konv") {
      case class Target(x: String)
      case class Source(x: Long = 1)
      test("can compile if dose not define implicit Konv") {
//        assert(compileError("""
//      From(Source()).to[Target] == Target("1")
//      """).msg.contains("type mismatch"))
      }
      test("can compile if defined implicit Konv") {
        implicit val x = Mapper[Long, String](x => s"implicit parameter $x")
        assert(From(Source()).to[Target] == Target("implicit parameter 1"))
      }
      test("can compile if defined implicit conversion") {
        implicit def x(x: Long) = s"implicit conversion $x"

        assert(From(Source()).to[Target] == Target("implicit conversion 1"))
      }
      test("use implicit parameter over implicit conversion") {
        implicit def ic(x: Long) = s"conversion $x"

        implicit val ip = Mapper[Long, String](_ => "parameter")
        val x = ic(1L)
        assert(x == "conversion 1")
        assert(From(Source()).to[Target] == Target("parameter"))
      }
    }
    test("warp unwrap single value class") {
      case class Target1(name: String)
      case class Target(x: Target1)
      case class Source1(x: String)
      test("auto wrap") {
        assert(From(Source1("1")).to[Target] == Target(Target1("1")))
      }
      test("auto unwrap") {
        assert(From(Target(Target1("1"))).to[Source1] == Source1("1"))
      }
      test("auto wrap2") {
        assert(From(Source1("1")).to[Target1] == Target1("1"))
      }
      test("auto unwrap2") {
        assert(From(Target1("1")).to[Source1] == Source1("1"))
      }
      test("auto wrap3") {
        assert(From("2").to[Target1] == Target1("2"))
      }
      test("auto unwrap3") {
        assert(From(Target1("1")).to[String] == "1")
      }
    }
    test("side effect") {
      test("test side effect") {
        case class Target(a: Int)
        var x = 10

        def i = {
          x += 1
          x
        }

        assert(From(i /* no use */).to[Target] == Target(11))
        assert(i == 12)
      }
      test("test underscore") {
        object UnderScore {
          case class Target(a: Int)
          def test: Target => Target = From(_).to[Target]
        }

        UnderScore.test(UnderScore.Target(10)) ==> UnderScore.Target(10)
      }
    }
    test("generic class") {
      case class S1(a: String, v: Int)
      case class Source[A](name: String, value: A)
      test("test generics") {
        case class T1(a: String, v: Int)
        case class Target[A](name: String, value: A)
        implicit val x = Mapper.mapper[S1, T1]
        assert(From(Source("x", S1("a", 1))).to[Target[T1]] == Target("x", T1("a", 1)))
      }
      test("generics 2") {
        case class T1[A](a: A, v: Int)
        case class Target[A](name: String, value: A)
        implicit val x = Mapper.mapper[S1, T1[String]]
        assert(From(Source("x", S1("a", 1))).to[Target[T1[String]]] == Target("x", T1("a", 1)))
      }
      test("generics inner class") {
        case class Source[A](name: String, value: A) {

          case class Target(name: String, value: A)

          def auto = From(this).to[Target]

          def manual = Target(this.name, this.value)
        }
        val s = Source[Int]("x", 1)
        assert(s.auto == s.manual)
      }
    }
  }
}
