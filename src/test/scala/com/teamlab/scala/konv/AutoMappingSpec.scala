package com.teamlab.scala.konv

import org.scalatest._


trait TestKonvs extends KonvDefaults {
  implicit val dataC = Konv.mapper[SourceData, TargetData]
  implicit val level2C = Konv.mapper[SourceLevel2, TargetLevel2]
  implicit val level1C = Konv.mapper[SourceLevel1, TargetLevel1]
  implicit val mainC = Konv.mapper[SourceClass, TargetClass]
  implicit val pClassAC = Konv.mapper[SourcePolymorphicClassA, TargetPolymorphicClassA]
  implicit val pClassBC = Konv.mapper[SourcePolymorphicClassB, TargetPolymorphicClassB]
}
class AutomapperSpec extends FunSpec with Matchers with TestData with TestKonvs {
  describe("automap"){

    it("map a case class to another case class as expected"){
      Konv.to[TargetClass].by(source) === target
    }

    it("map a case class with missing optionals to another case class as expected"){
      val sourceWithMissingOptionals = SourceClass("field", sourceData, sourceValues, sourceDatas, None, None, sourceMap, sourceMapWithData, sourceLevel1)
      val targetWithMissingOptionals = TargetClass("field", targetData, targetValues, targetDatas, None, None, targetMap, targetMapWithData, targetLevel1)
      Konv.to[TargetClass].by(sourceWithMissingOptionals) === targetWithMissingOptionals
    }

    it("map a case class to another case class with a subset of fields"){
      implicit val v = Konv.mapper[SourceClass, TargetSubset]
      Konv.to[TargetSubset].by(source) === TargetSubset(targetData)
    }
//    it("map a case class to another case class by setting None for fields not present in the first class"){
//      implicit val v2 = Konv.caseClass[SourceClass, TargetWithOptionalUnexpectedField]
//      Konv.from(source).to[TargetWithOptionalUnexpectedField] === TargetWithOptionalUnexpectedField(targetData, None)
//    }
//
//    it("map a case class to another case class by setting an empty iterable for fields not present in the first class"){
//      implicit val v2 = Konv.caseClass[SourceClass, TargetWithUnexpectedList]
//      Konv.from(source).to[TargetWithUnexpectedList] === TargetWithUnexpectedList(targetData, List.empty)
//    }
//
//    it("map a case class to another case class by setting an empty map for fields not present in the first class"){
//      Konv.from(source).to[TargetWithUnexpectedMap] === TargetWithUnexpectedMap(targetData, Map.empty)
//    }

    it("map a case class to another case class by setting the default value for fields not present in the first class"){
      implicit val v2 = Konv.mapper[SourceClass, TargetWithDefaultValue]
      Konv.to[TargetWithDefaultValue].by(source) === TargetWithDefaultValue(targetData)
    }

    it("map a case class to another case class when using a qualified type"){
      implicit val v1 = Konv.mapper[SomeObject.Data, AnotherObject.Data]
      implicit val v2 = Konv.mapper[SomeObject.Source, AnotherObject.Target]
      Konv.to[AnotherObject.Target].by(SomeObject.Source("value", SomeObject.Data(1))) === AnotherObject.Target("value", AnotherObject.Data(1))
    }

    it("not compile if mapping cannot be generated"){
      "Konv.to[TargetWithUnexpectedField].by(source)" shouldNot compile
    }
  }

  describe("set"){
    val values = source.list
    def sum(values: List[Int]) = values.sum
    it("map a case class to another case class allowing dynamic fields mapping"){
      Konv.to[TargetWithDynamicMapping].by(source,
        renamedField = source.field,
        total = sum(values)
      ) === TargetWithDynamicMapping("field", targetData, 6)
    }

    it("not compile if missing mappings have not been provided in the dynamic mapping"){
      """
      Konv.to[TargetWithDynamicMapping].by(source,
        renamedField = source.field
      )
      """ shouldNot compile
    }

    it("not compile if typechecking fails when assigning a field dynamically"){
      """
      Konv.to[TargetWithDynamicMapping].by(source,
        renamedField = 10,
        total = "value"
      )
      """ shouldNot compile
    }
    it("not compile if parems too many"){
      """
      Konv.to[TargetWithDynamicMapping].by(source,
        renamedField = source.field,
        total = sum(values),
        aaa = 100
      )
      """ shouldNot compile
    }
  }

  describe("automap using generated implicit mappings"){
    it("map a case class to another case class as expected using the manually generated implicit mappings"){
//      implicit val mapping = Konv.caseClass[SourceClass, TargetClass]
      Konv.to[TargetClass].by(source) === target
    }

    it("map a case class to another case class as expected using the manually generated implicit mappings and be able to disambiguate between multiple implicit mappings"){
//      implicit val mapping = Konv.caseClass[SourceClass, TargetClass]
//      implicit val mappingForSubset = Konv.caseClass[SourceClass, TargetSubset]
      Konv.to[TargetClass].by(source) === target
    }
  }

  describe("automap polymorphic types"){
    def mapPolymorphicTrait(source: SourcePolymorphicTrait): TargetPolymorphicTrait = source match {
      case a: SourcePolymorphicClassA => Konv.to[TargetPolymorphicClassA].by(a)
      case b: SourcePolymorphicClassB => Konv.to[TargetPolymorphicClassB].by(b)
    }

    it("map a polymorphic type field"){
      implicit val conversion = Konv(mapPolymorphicTrait)
      implicit val conversion2 = Konv.mapper[SourcePolymorphicClass, TargetPolymorphicClass]
      Konv.to[TargetPolymorphicClass].by(sourcePolymorphicA) === targetPolymorphicA
      Konv.to[TargetPolymorphicClass].by(sourcePolymorphicB) === targetPolymorphicB
    }

    it("throw an exception for an unmapped polymorphic type"){
      assertThrows[MatchError] {
          implicit val conversion = Konv(mapPolymorphicTrait)
        implicit val conversion2 = Konv.mapper[SourcePolymorphicClass, TargetPolymorphicClass]
          Konv.to[TargetPolymorphicClass].by(sourcePolymorphicC)
      }
    }

    it("not compile without an implicit conversion in scope"){
      "Konv.to[TargetPolymorphicClass].by(sourcePolymorphicA)" shouldNot compile
    }

    it("useing implicit mapping"){
      implicit val implicitMapping = Konv[SourceClass, TargetWithDynamicMapping]{ a:SourceClass =>
        Konv.to[TargetWithDynamicMapping].by(a, renamedField = a.field, total = a.list.sum)
      }
      implicit val conversion2 = Konv.mapper[SourceClass1, TargetClass1]
      implicitMapping.map(sourceClass1.value) === targetClass1.value
      //      """ Konv.from(sourceClass1).to[TargetClass1] === targetClass1 """ shouldNot compile
      Konv.to[TargetClass1].by(sourceClass1) === targetClass1
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
  level1: SourceLevel1)

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
  level1: TargetLevel1)

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
      "field", sourceData,
      sourceValues, sourceDatas,
      Some("optional"), Some(sourceData),
      sourceMap, sourceMapWithData,
      sourceLevel1)

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
      "field", targetData,
      targetValues, targetDatas,
      Some("optional"), Some(targetData),
      targetMap, targetMapWithData,
      targetLevel1)

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
