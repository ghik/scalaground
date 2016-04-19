package com.avsystem.demo

import com.avsystem.commons._

/**
  * Created by ghik on 18.04.16.
  */
trait TypeClass[T]
object TypeClass {
  implicit def specific: TypeClass[String] = ???
  implicit def default[T]: TypeClass[T] = ???
}

object Test {
  implicitly[TypeClass[String]]
}
