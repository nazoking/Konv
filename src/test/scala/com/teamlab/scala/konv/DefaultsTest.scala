package com.teamlab.scala.konv

import utest._

object DefaultsTest extends TestSuite with Defaults {
  def tests = Tests {
    test(" map") {
      implicitly[Mapper[Map[String, Int], Map[String, Long]]].map(Map("1" -> 1)) ==> Map("1" -> 1L)
    }
    test("same class") {
      implicitly[Mapper[A, A]].map(A()) ==> A()
    }
    test(" extends") {
      implicitly[Mapper[Ext, Base]].map(new Ext())
    }
    test("now low") {
      compileError("""
    implicitly[Mapper[Base, Ext]]
    """).check("", "could not find implicit value for parameter")
    }
    test("list same item") {
      implicitly[Mapper[List[Int], Set[Int]]].map(List(1, 2)) ==> Set(1, 2)
    }
    test("list othre item use impliict Konv") {
      implicit val intTOString = Mapper[Int, String](_.toString)
      implicitly[Mapper[List[Int], Set[Int]]].map(List(1, 2)) ==> Set(1, 2)
      implicitly[Mapper[List[Int], Set[String]]].map(List(1, 2)) ==> Set("1", "2")
    }
    test("map othre item use impliict Konv") {
      implicit val intTOString = Mapper[Int, String](_.toString)
      assert(
        implicitly[Mapper[Map[String, Int], Map[String, String]]]
          .map(Map("1" -> 1, "2" -> 2)) == Map("1" -> "1", "2" -> "2")
      )
    }
    test("map option use impliict Konv") {
      implicit val intTOString = Mapper[Int, String](_.toString)
      implicitly[Mapper[Option[Int], Option[Int]]].map(Some(1)) ==> Some(1)
      implicitly[Mapper[Option[Int], Option[String]]].map(Some(1)) ==> Some("1")
      implicitly[Mapper[Option[Int], Option[String]]].map(None) ==> None
    }
    test("object to Some") {
      implicit val intTOString = Mapper[Int, String](_.toString)
      assert(implicitly[Mapper[Int, Option[String]]].map(1) == Some("1"))
    }
  }

  class Base

  class Ext extends Base

  case class A(name: String = "test")

}
