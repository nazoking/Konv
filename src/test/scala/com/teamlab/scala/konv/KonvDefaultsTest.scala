package com.teamlab.scala.konv

import org.scalatest.{DiagrammedAssertions, FunSpec, FunSuite}

class KonvDefaultsTest extends FunSpec with KonvDefaults with DiagrammedAssertions{
  import KonvDefaultsTest._
  it("map"){
    assert(implicitly[Konv[Map[String, Int], Map[String, Long]]].map(Map("1"->1)) == Map("1"->1L))
  }
  it("same class"){
    assert(implicitly[Konv[A, A]].map(A()) == A())
  }
  it("extends"){
    implicitly[Konv[Ext, Base]].map(new Ext())
  }
  it("now low"){
    assertDoesNotCompile("""
    implicitly[Konv[Base, Ext]]
    """)
  }
  it("list same item"){
    assert(implicitly[Konv[List[Int], Set[Int]]].map(List(1, 2)) == Set(1, 2))
  }
  it("list othre item use impliict Konv"){
    implicit val intTOString = Konv[Int, String](_.toString)
    assert(implicitly[Konv[List[Int], Set[Int]]].map(List(1, 2)) == Set(1, 2))
//    assert(implicitly[Konv[List[Int], List[Int]]].map(List(1, 2)) == Set(1, 2))
  }
  it("map othre item use impliict Konv"){
    implicit val intTOString = Konv[Int, String](_.toString)
    assert(implicitly[Konv[Map[String, Int], Map[String, String]]].map(Map("1"->1, "2"->2)) == Map("1"->"1","2"->"2"))
  }
  it("map option use impliict Konv"){
    implicit val intTOString = Konv[Int, String](_.toString)
    assert(implicitly[Konv[Option[Int], Option[Int]]].map(Some(1)) == Some(1))
    assert(implicitly[Konv[Option[Int], Option[String]]].map(Some(1)) == Some("1"))
    assert(implicitly[Konv[Option[Int], Option[String]]].map(None) == None)
  }
  it("object to Some"){
    implicit val intTOString = Konv[Int, String](_.toString)
    assert(implicitly[Konv[Int, Option[String]]].map(1) == Some("1"))
  }
}
object KonvDefaultsTest {

  class Base

  class Ext extends Base

  case class A(name: String = "test")

}