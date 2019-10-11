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

You can use `Konv.to`.

```scala
// from
case class User(firstName: String, lastName:String, age:Int, email:String)

// to
case class UserForApi(name: String, age:Int, email:String)

val user = User("taro", "suzuki", 12, "xxx@example.com")

import com.teamlab.scala.konv.Konv

Konv.to[UserForApi].by(user, name=s"${user.firstName} ${user.lastName}")
// => UserForApi("taro suzuki", 12, "xxx@example.com")
```

auto mapping parameters from source object same name fields. or using named parameter of `by` method.

  1. `Konv.to` set target
    - `Konv.to[TYPE]` generate by primary constructor
    - `Konv.to(Factory.apply _)` generate by factory method
    - `Konv.to(CompanionObject)` is same as `Konv.to(CompanionObject.apply _)`
  2. `by` set source and generate.
    - `by(sourceObject)`
    - `by(sourceObject, param1=overwrite)` overwrite parameters.
        - use for parameter rename
        - use for parameter type change
        - use for some calculation

### create custom module

`trait Konv[A, B]` is converter module (A to B). `konv` use type class.

```scala
import com.teamlab.scala.konv.Konv
case class S1(field: Int)
case class T1(field: String)

case class Source(value:S1)
case class Target(value:T1)

{
  Konv.to(Target).by(Source(S1(10))) // => error
}

{
  // create module type class
  implicit val convert = Konv[S1, T1](s: S1 => T1(s.field.toString))

  // Konv.to use implicit Konv trait if source property class differ to target parameter class
  Konv.to(Target).by(Source(S1(10)))
  // Target(T1("10"))
}
```

and you can use default converters defined on `KonvDefaults`.

