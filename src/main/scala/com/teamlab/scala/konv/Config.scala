package com.teamlab.scala.konv

trait Config
object Config {
  final class Empty extends Config
  final class GetCode[C <: Config] extends Config
}
