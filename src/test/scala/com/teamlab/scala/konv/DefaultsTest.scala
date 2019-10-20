package com.teamlab.scala.konv

import utest._

object DefaultsTest extends TestSuite with Defaults {
  def tests = Tests {
    " map" - {
      assert(implicitly[Mapper[Map[String, Int], Map[String, Long]]].map(Map("1" -> 1)) == Map("1" -> 1L))
    }
    "same class" - {
      assert(implicitly[Mapper[A, A]].map(A()) == A())
    }
    " extends" - {
      implicitly[Mapper[Ext, Base]].map(new Ext())
    }
    "now low" - {
      compileError("""
    implicitly[Konv[Base, Ext]]
    """)
    }
    "list same item" - {
      assert(implicitly[Mapper[List[Int], Set[Int]]].map(List(1, 2)) == Set(1, 2))
    }
    "list othre item use impliict Konv" - {
      implicit val intTOString = Mapper[Int, String](_.toString)
      assert(implicitly[Mapper[List[Int], Set[Int]]].map(List(1, 2)) == Set(1, 2))
      assert(implicitly[Mapper[List[Int], Set[String]]].map(List(1, 2)) == Set("1", "2"))
    }
    "map othre item use impliict Konv" - {
      implicit val intTOString = Mapper[Int, String](_.toString)
      assert(
        implicitly[Mapper[Map[String, Int], Map[String, String]]]
          .map(Map("1" -> 1, "2" -> 2)) == Map("1" -> "1", "2" -> "2")
      )
    }
    "map option use impliict Konv" - {
      implicit val intTOString = Mapper[Int, String](_.toString)
      assert(implicitly[Mapper[Option[Int], Option[Int]]].map(Some(1)) == Some(1))
      assert(implicitly[Mapper[Option[Int], Option[String]]].map(Some(1)) == Some("1"))
      assert(implicitly[Mapper[Option[Int], Option[String]]].map(None) == None)
    }
    "object to Some" - {
      implicit val intTOString = Mapper[Int, String](_.toString)
      assert(implicitly[Mapper[Int, Option[String]]].map(1) == Some("1"))
    }
  }

  class Base

  class Ext extends Base

  case class A(name: String = "test")

}
