package com.teamlab.scala.konv

import org.scalatest.DiagrammedAssertions
import org.scalatest.refspec.RefSpec

class KonvDefaultsTest extends RefSpec with KonvDefaults with DiagrammedAssertions {
  import KonvDefaultsTest._
  def ` map`(): Unit = {
    assert(implicitly[Konv[Map[String, Int], Map[String, Long]]].map(Map("1" -> 1)) == Map("1" -> 1L))
  }
  def `same class`(): Unit = {
    assert(implicitly[Konv[A, A]].map(A()) == A())
  }
  def ` extends`(): Unit = {
    implicitly[Konv[Ext, Base]].map(new Ext())
  }
  def `now low`(): Unit = {
    assertDoesNotCompile("""
    implicitly[Konv[Base, Ext]]
    """)
  }
  def `list same item`(): Unit = {
    assert(implicitly[Konv[List[Int], Set[Int]]].map(List(1, 2)) == Set(1, 2))
  }
  def `list othre item use impliict Konv`(): Unit = {
    implicit val intTOString = Konv[Int, String](_.toString)
    assert(implicitly[Konv[List[Int], Set[Int]]].map(List(1, 2)) == Set(1, 2))
    assert(implicitly[Konv[List[Int], Set[String]]].map(List(1, 2)) == Set("1", "2"))
  }
  def `map othre item use impliict Konv`(): Unit = {
    implicit val intTOString = Konv[Int, String](_.toString)
    assert(
      implicitly[Konv[Map[String, Int], Map[String, String]]]
        .map(Map("1" -> 1, "2" -> 2)) == Map("1" -> "1", "2" -> "2")
    )
  }
  def `map option use impliict Konv`(): Unit = {
    implicit val intTOString = Konv[Int, String](_.toString)
    assert(implicitly[Konv[Option[Int], Option[Int]]].map(Some(1)) == Some(1))
    assert(implicitly[Konv[Option[Int], Option[String]]].map(Some(1)) == Some("1"))
    assert(implicitly[Konv[Option[Int], Option[String]]].map(None) == None)
  }
  def `object to Some`(): Unit = {
    implicit val intTOString = Konv[Int, String](_.toString)
    assert(implicitly[Konv[Int, Option[String]]].map(1) == Some("1"))
  }
}
object KonvDefaultsTest {

  class Base

  class Ext extends Base

  case class A(name: String = "test")

}
