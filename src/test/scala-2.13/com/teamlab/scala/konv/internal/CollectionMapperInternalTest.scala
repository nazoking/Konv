package com.teamlab.scala.konv.internal

import com.teamlab.scala.konv.Mapper
import utest._

object CollectionMapperInternalTest extends TestSuite with CollectionMapper {
  trait Target
  trait Extends extends Target
  trait Other
  type TargetC[A] = Seq[A]
  type OtherC[A] = Set[A]
  type ExtendC[A] = List[A]
  def typeOk[A, B](a: Mapper[A, B]): Mapper[A, B] = a
  def dummyMapper[A, B]: Mapper[A, B] = Mapper { _: A =>
    ???
  }
  val EVIDENCE_EXTENDED = "<:!<"
  val EXPANSION = "diverging implicit expansion"
  // Seq <: List
  override def tests = Tests {
    test("default_listTo") {
      test("other container, same element") - typeOk[List[Target], Set[Target]](default_listTo)
      test("other container, base element") - typeOk[List[Extends], Set[Target]](default_listTo)
      test("other container, other element") - compileError("""
        typeOk[List[Other], Set[Target]](default_listTo)
          """).check("", EVIDENCE_EXTENDED)
      test("same container, same element") - compileError("""
        typeOk[List[Target], List[Target]](default_listTo)
          """).check("", EVIDENCE_EXTENDED)
      test("same container, base element") - compileError("""
        typeOk[List[Extends], List[Target]](default_listTo)
          """).check("", EVIDENCE_EXTENDED)
      test("same container, other element") - compileError("""
        typeOk[List[Other], List[Target]](default_listTo)
          """).check("", EVIDENCE_EXTENDED)
      test("base container, same element") - compileError("""
        typeOk[List[Target], Seq[Target]](default_listTo)
          """).check("", EVIDENCE_EXTENDED)
      test("base container, base element") - compileError("""
        typeOk[List[Extends], Seq[Target]](default_listTo)
          """).check("", EVIDENCE_EXTENDED)
      test("base container, other element") - compileError("""
        typeOk[List[Extends], Seq[Target]](default_listTo)
          """).check("", EVIDENCE_EXTENDED)
    }
    test("default_iterable") {
      implicit val x: Mapper[Other, Target] = dummyMapper
      test("other container, same element") - compileError("""
        typeOk[List[Target], Set[Target]](default_iterable)
          """).check("", EXPANSION)
      test("other container, base element") - compileError("""
        typeOk[List[Extends], Set[Target]](default_iterable)
          """).check("", EXPANSION)
      test("other container, other element") - typeOk[List[Other], Set[Target]](default_iterable)
      test("same container, same element") - compileError("""
        typeOk[List[Target], List[Target]](default_iterable)
          """).check("", EXPANSION)
      test("same container, base element") - compileError("""
        typeOk[List[Extends], List[Target]](default_iterable)
          """).check("", EXPANSION)
      test("same container, other element") - typeOk[List[Other], List[Target]](default_iterable)
      test("base container, same element") - compileError("""
        typeOk[List[Target], Seq[Target]](default_iterable)
          """).check("", EXPANSION)
      test("base container, base element") - compileError("""
        typeOk[List[Extends], Seq[Target]](default_iterable)
          """).check("", EXPANSION)
      test("base container, other element") - compileError("""
        typeOk[List[Extends], Seq[Target]](default_iterable)
          """).check("", EXPANSION)
    }
  }
}
