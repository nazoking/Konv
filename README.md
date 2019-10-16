Konv
===============

type safe auto-mapping framework scala.

 * easy to use
 * type safe (compile-time type check)
 * customizable
   * type class base definition

Usage
-----

### simple convert with modifing field

You can use `From(xx).to[B]`.

```scala
// from
case class User(firstName: String, lastName:String, age:Int, email:String)

// to
case class UserForApi(name: String, age:Int, email:String)

val user = User("taro", "suzuki", 12, "xxx@example.com")

import com.teamlab.scala.konv.From

From(user, name=s"${user.firstName} ${user.lastName}").to[UserForApi]
// => UserForApi("taro suzuki", 12, "xxx@example.com")
```

auto mapping parameters from source object same name fields. or using named parameter of `by` method.

  1. `From` set source and generate.
     - `From(sourceObject)`
     - `From(sourceObject, param1=overwrite)` overwrite parameters.
        - use for parameter rename
        - use for parameter type change
        - use for some calculation
  2. `to` set target, and get
     - `to[TYPE]` generate by primary constructor
     - `to(Factory.apply _)` generate by factory method
     - `to(CompanionObject)` is same as `to(CompanionObject.apply _)`

### create custom module

`trait Konv[A, B]` is converter module (A to B). `konv` use type class.

```scala
import com.teamlab.scala.konv.{From, Konv}
case class S1(field: Int)
case class T1(field: String)

case class Source(value:S1)
case class Target(value:T1)

{
  From(Source(S1(10))).to(Target) // => error
}

{
  // create module type class
  implicit val convert = Konv[S1, T1](s: S1 => T1(s.field.toString))

  // Konv.to use implicit Konv trait if source property class differ to target parameter class
  From(Source(S1(10))).to(Target)
  // Target(T1("10"))
}
```

and you can use default converters defined on `KonvDefaults`.

Using Rules
-----------

  1. overwrite parameters if exists
  2. same name field if exists in source
    * if type is different, try below
      1. try `implicit val Konv[A, B].map(source)`
      2. try `new Target(source)`
      3. if source type is single-value-case-class and parameter type is it, use `source.value`
      4. and `new Target(source.value)`
  3. default value if exists, not set


Related Projects
----------------

  * https://mapstruct.org/
  * http://modelmapper.org/
  * http://jmapper-framework.github.io/jmapper-core/
  * https://github.com/bfil/scala-automapper Hassle-free case class mapping!
  * https://scalalandio.github.io/chimney/ Scala library for boilerplate-free data transformations


