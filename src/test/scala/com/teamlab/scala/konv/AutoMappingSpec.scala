package com.teamlab.scala.konv

import org.scalatest._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.refspec.RefSpec

trait TestKonvs extends KonvDefaults {
  implicit val dataC = Konv.mapper[SourceData, TargetData]
  implicit val level2C = Konv.mapper[SourceLevel2, TargetLevel2]
  implicit val level1C = Konv.mapper[SourceLevel1, TargetLevel1]
  implicit val mainC = Konv.mapper[SourceClass, TargetClass]
  implicit val pClassAC = Konv.mapper[SourcePolymorphicClassA, TargetPolymorphicClassA]
  implicit val pClassBC = Konv.mapper[SourcePolymorphicClassB, TargetPolymorphicClassB]
}
class AutomapperSpec extends RefSpec with Matchers with TestData with TestKonvs {
  object `automap ` {

    def `map a case class to another case class as expected`(): Unit = {
      assert(From(source).to[TargetClass] === target)
    }

    def `map a case class with missing optionals to another case class as expected`(): Unit = {
      val sourceWithMissingOptionals = SourceClass(
        "field",
        sourceData,
        sourceValues,
        sourceDatas,
        None,
        None,
        sourceMap,
        sourceMapWithData,
        sourceLevel1
      )
      val targetWithMissingOptionals = TargetClass(
        "field",
        targetData,
        targetValues,
        targetDatas,
        None,
        None,
        targetMap,
        targetMapWithData,
        targetLevel1
      )
      assert(From(sourceWithMissingOptionals).to[TargetClass] === targetWithMissingOptionals)
    }

    def `map a case class to another case class with a subset of fields`(): Unit = {
//      implicit val v = Konv.mapper[SourceClass, TargetSubset]
      assert(From(source).to[TargetSubset] === TargetSubset(targetData))
    }
//    def `map a case class to another case class by setting None for fields not present in the first class`(): Unit = {
//      implicit val v2 = Konv.caseClass[SourceClass, TargetWithOptionalUnexpectedField]
//      From(source).to[TargetWithOptionalUnexpectedField] === TargetWithOptionalUnexpectedField(targetData, None)
//    }
//
//    def `map a case class to another case class by setting an empty iterable for fields not present in the first class`(): Unit ={
//      implicit val v2 = Konv.caseClass[SourceClass, TargetWithUnexpectedList]
//      From(source).to[TargetWithUnexpectedList] === TargetWithUnexpectedList(targetData, List.empty)
//    }
//
//    def `map a case class to another case class by setting an empty map for fields not present in the first class`(): Unit ={
//      From(source).to[TargetWithUnexpectedMap] === TargetWithUnexpectedMap(targetData, Map.empty)
//    }

    def `map a case class to another case class by setting the default value for fields not present in the first class`()
        : Unit = {
//      implicit val v2 = Konv.mapper[SourceClass, TargetWithDefaultValue]
      assert(From(source).to[TargetWithDefaultValue] === TargetWithDefaultValue(targetData))
    }

    def `map a case class to another case class when using a qualified type`(): Unit = {
//      implicit val v1 = Konv.mapper[SomeObject.Data, AnotherObject.Data]
//      implicit val v2 = Konv.mapper[SomeObject.Source, AnotherObject.Target]
      assert(
        From(SomeObject.Source("value", SomeObject.Data(1)))
          .to[AnotherObject.Target] === AnotherObject.Target("value", AnotherObject.Data(1))
      )
    }

    def `not compile if mapping cannot be generated`(): Unit = {
      assert(intercept[TestFailedException](assertCompiles("""
        From(source).to[TargetWithUnexpectedField]
      """)).getMessage().contains("not enough arguments"))
    }
  }

  object `set ` {
    val values = source.list
    def sum(values: List[Int]) = values.sum
    def `map a case class to another case class allowing dynamic fields mapping`(): Unit = {
      assert(
        From(source, renamedField = source.field, total = sum(values))
          .to[TargetWithDynamicMapping] === TargetWithDynamicMapping("field", targetData, 6)
      )
    }

    def `not compile if missing mappings have not been provided in the dynamic mapping`(): Unit = {
      assert(intercept[TestFailedException](assertCompiles("""
      From(source,
        renamedField = source.field
      ).to[TargetWithDynamicMapping]
      """)).getMessage().contains("not enough arguments"))
    }

    def `not compile if typechecking fails when assigning a field dynamically`(): Unit = {
      assert(intercept[TestFailedException](assertCompiles("""
      From(source,
        renamedField = 10,
        total = "value"
      ).to[TargetWithDynamicMapping]
      """)).getMessage().contains("type mismatch"))
    }
    def `not compile if parems too many`(): Unit = {
      assert(intercept[TestFailedException](assertCompiles("""
      From(source,
        renamedField = source.field,
        total = sum(values),
        aaa = 100
      ).to[TargetWithDynamicMapping]
      """)).getMessage().contains("is not in "))
    }
  }

  object `automap using generated implicit mappings` {
    def `map a case class to another case class as expected using the manually generated implicit mappings`(): Unit = {
//      implicit val mapping = Konv.caseClass[SourceClass, TargetClass]
      assert(From(source).to[TargetClass] === target)
    }

    def `map a case class to another case class as expected using the manually generated implicit mappings and be able to disambiguate between multiple implicit mappings`()
        : Unit = {
//      implicit val mapping = Konv.caseClass[SourceClass, TargetClass]
//      implicit val mappingForSubset = Konv.caseClass[SourceClass, TargetSubset]
      assert(From(source).to[TargetClass] === target)
    }
  }

  object `automap polymorphic types` {
    def mapPolymorphicTrait(source: SourcePolymorphicTrait): TargetPolymorphicTrait = source match {
      case a: SourcePolymorphicClassA => From(a).to[TargetPolymorphicClassA]
      case b: SourcePolymorphicClassB => From(b).to[TargetPolymorphicClassB]
    }

    def `map a polymorphic type field`(): Unit = {
      implicit val conversion = Konv(mapPolymorphicTrait)
//      implicit val conversion2 = Konv.mapper[SourcePolymorphicClass, TargetPolymorphicClass]
      assert(From(sourcePolymorphicA).to[TargetPolymorphicClass] === targetPolymorphicA)
      assert(From(sourcePolymorphicB).to[TargetPolymorphicClass] === targetPolymorphicB)
    }

    def `throw an exception for an unmapped polymorphic type`(): Unit = {
      implicit val conversion = Konv(mapPolymorphicTrait)
//      implicit val conversion2 = Konv.mapper[SourcePolymorphicClass, TargetPolymorphicClass]
      assertThrows[MatchError] {
        From(sourcePolymorphicC).to[TargetPolymorphicClass]
      }
    }

    def `not compile without an implicit conversion in scope`(): Unit = {
      assert(intercept[TestFailedException](assertCompiles("""
      From(sourcePolymorphicA).to[TargetPolymorphicClass]
      """)).getMessage().contains("type mismatch"))
    }

    def `useing implicit mapping`(): Unit = {
      implicit val implicitMapping = Konv[SourceClass, TargetWithDynamicMapping] { a: SourceClass =>
        From(a, renamedField = a.field, total = a.list.sum).to[TargetWithDynamicMapping]
      }
//      implicit val conversion2 = Konv.mapper[SourceClass1, TargetClass1]
      assert(implicitMapping.map(sourceClass1.value) === targetClass1.value)
      //      """ Konv.from(sourceClass1).to[TargetClass1] === targetClass1 """ shouldNot compile
      assert(From(sourceClass1).to[TargetClass1] === targetClass1)
    }
  }
}

case class SourceClass(
    field: String,
    data: SourceData,
    list: List[Int],
    typedList: List[SourceData],
    optional: Option[String],
    typedOptional: Option[SourceData],
    map: Map[String, Int],
    typedMap: Map[String, SourceData],
    level1: SourceLevel1
)

case class SourceData(label: String, value: Int)
case class SourceLevel1(level2: Option[SourceLevel2])
case class SourceLevel2(treasure: String)

trait SourcePolymorphicTrait
case class SourcePolymorphicClassA(label: String, value: Int) extends SourcePolymorphicTrait
case class SourcePolymorphicClassB(width: Int) extends SourcePolymorphicTrait
case class SourcePolymorphicClassC(title: String) extends SourcePolymorphicTrait
case class SourcePolymorphicClass(field: SourcePolymorphicTrait)

case class TargetClass(
    field: String,
    data: TargetData,
    list: List[Int],
    typedList: List[TargetData],
    optional: Option[String],
    typedOptional: Option[TargetData],
    map: Map[String, Int],
    typedMap: Map[String, TargetData],
    level1: TargetLevel1
)

case class TargetData(label: String, value: Int)
case class TargetLevel1(level2: Option[TargetLevel2])
case class TargetLevel2(treasure: String)

case class TargetSubset(data: TargetData)
case class TargetWithUnexpectedField(data: TargetData, unexpectedField: Exception)
case class TargetWithOptionalUnexpectedField(data: TargetData, unexpectedField: Option[Exception])
case class TargetWithUnexpectedList(data: TargetData, unexpectedList: List[Int])
case class TargetWithUnexpectedMap(data: TargetData, unexpectedMap: Map[String, Int])
case class TargetWithDefaultValue(data: TargetData, default: String = "default")
case class TargetWithDynamicMapping(renamedField: String, data: TargetData, total: Int)

trait TargetPolymorphicTrait
case class TargetPolymorphicClassA(label: String, value: Int) extends TargetPolymorphicTrait
case class TargetPolymorphicClassB(width: Int) extends TargetPolymorphicTrait
case class TargetPolymorphicClass(field: TargetPolymorphicTrait)

case class SourceClass1(value: SourceClass)
case class TargetClass1(value: TargetWithDynamicMapping)

trait TestData {

  val sourceData = SourceData("label", 10)
  val sourceLevel2 = SourceLevel2("treasure")
  val sourceLevel1 = SourceLevel1(Some(sourceLevel2))

  val sourceValues = List(1, 2, 3)
  val sourceDatas = List(SourceData("label1", 1), SourceData("label1", 2), SourceData("label1", 3))
  val sourceMap = Map("one" -> 1, "two" -> 2)
  val sourceMapWithData = Map("one" -> SourceData("label1", 1), "two" -> SourceData("label2", 2))

  val source =
    SourceClass(
      "field",
      sourceData,
      sourceValues,
      sourceDatas,
      Some("optional"),
      Some(sourceData),
      sourceMap,
      sourceMapWithData,
      sourceLevel1
    )

  val sourcePolymorphicA = SourcePolymorphicClass(SourcePolymorphicClassA("label", 10))
  val sourcePolymorphicB = SourcePolymorphicClass(SourcePolymorphicClassB(11))
  val sourcePolymorphicC = SourcePolymorphicClass(SourcePolymorphicClassC("title"))

  val targetData = TargetData("label", 10)
  val targetLevel2 = TargetLevel2("treasure")
  val targetLevel1 = TargetLevel1(Some(targetLevel2))

  val targetValues = List(1, 2, 3)
  val targetDatas = List(TargetData("label1", 1), TargetData("label1", 2), TargetData("label1", 3))
  val targetMap = Map("one" -> 1, "two" -> 2)
  val targetMapWithData = Map("one" -> TargetData("label1", 1), "two" -> TargetData("label2", 2))

  val target =
    TargetClass(
      "field",
      targetData,
      targetValues,
      targetDatas,
      Some("optional"),
      Some(targetData),
      targetMap,
      targetMapWithData,
      targetLevel1
    )

  val targetPolymorphicA = TargetPolymorphicClass(TargetPolymorphicClassA("label", 10))
  val targetPolymorphicB = TargetPolymorphicClass(TargetPolymorphicClassB(11))

  val sourceClass1 = SourceClass1(source)
  val targetClass1 = TargetClass1(TargetWithDynamicMapping("field", targetData, 6))

}

object SomeObject {
  case class Source(value: String, data: Data)
  case class Data(value: Int)
}

object AnotherObject {
  case class Target(value: String, data: Data)
  case class Data(value: Int)
}
