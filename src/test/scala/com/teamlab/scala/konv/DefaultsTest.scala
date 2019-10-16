package com.teamlab.scala.konv

import org.scalatest.DiagrammedAssertions
import org.scalatest.refspec.RefSpec

class DefaultsTest extends RefSpec with Defaults with DiagrammedAssertions {
  import DefaultsTest._
  def ` map`(): Unit = {
    assert(implicitly[Mapper[Map[String, Int], Map[String, Long]]].map(Map("1" -> 1)) == Map("1" -> 1L))
  }
  def `same class`(): Unit = {
    assert(implicitly[Mapper[A, A]].map(A()) == A())
  }
  def ` extends`(): Unit = {
    implicitly[Mapper[Ext, Base]].map(new Ext())
  }
  def `now low`(): Unit = {
    assertDoesNotCompile("""
    implicitly[Konv[Base, Ext]]
    """)
  }
  def `list same item`(): Unit = {
    assert(implicitly[Mapper[List[Int], Set[Int]]].map(List(1, 2)) == Set(1, 2))
  }
  def `list othre item use impliict Konv`(): Unit = {
    implicit val intTOString = Mapper[Int, String](_.toString)
    assert(implicitly[Mapper[List[Int], Set[Int]]].map(List(1, 2)) == Set(1, 2))
    assert(implicitly[Mapper[List[Int], Set[String]]].map(List(1, 2)) == Set("1", "2"))
  }
  def `map othre item use impliict Konv`(): Unit = {
    implicit val intTOString = Mapper[Int, String](_.toString)
    assert(
      implicitly[Mapper[Map[String, Int], Map[String, String]]]
        .map(Map("1" -> 1, "2" -> 2)) == Map("1" -> "1", "2" -> "2")
    )
  }
  def `map option use impliict Konv`(): Unit = {
    implicit val intTOString = Mapper[Int, String](_.toString)
    assert(implicitly[Mapper[Option[Int], Option[Int]]].map(Some(1)) == Some(1))
    assert(implicitly[Mapper[Option[Int], Option[String]]].map(Some(1)) == Some("1"))
    assert(implicitly[Mapper[Option[Int], Option[String]]].map(None) == None)
  }
  def `object to Some`(): Unit = {
    implicit val intTOString = Mapper[Int, String](_.toString)
    assert(implicitly[Mapper[Int, Option[String]]].map(1) == Some("1"))
  }
}
object DefaultsTest {

  class Base

  class Ext extends Base

  case class A(name: String = "test")

}
