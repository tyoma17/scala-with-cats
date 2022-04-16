package chapter1_introduction

import chapter1_introduction.TheTypeClass.{Json, JsonWriter}
import chapter1_introduction.TypeClassInstances.Person

object TypeClassUse extends App {

  object Json { // interface object
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = // type class use
      w.write(value)
  }

  object JsonSyntax { // interface syntax
    implicit class JsonWriterOps[A](value: A) {
      def toJson(implicit w: JsonWriter[A]): Json =
        w.write(value)
    }
  }

  import JsonWriterInstances._

  val davePerson = Person("Dave", "dave@example.com")
  Json.toJson(davePerson)
  // JsObject(Map(name -> JsString(Dave), email -> JsString(dave@example.com)))

  Json.toJson(2.2)
  // JsNumber(2.2)

  import JsonSyntax.JsonWriterOps
  davePerson.toJson
  // JsObject(Map(name -> JsString(Dave), email -> JsString(dave@example.com)))

  val explicitJsonWriterString: JsonWriter[String] = implicitly[JsonWriter[String]]

  Option("a string").toJson
  // JsString(a string)

  val noneString: Option[String] = None
  noneString.toJson
  // JsNull
}
