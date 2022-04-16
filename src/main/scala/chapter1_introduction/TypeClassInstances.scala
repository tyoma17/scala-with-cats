package chapter1_introduction

import chapter1_introduction.TheTypeClass.{JsNull, JsNumber, JsObject, JsString, Json, JsonWriter}
import chapter1_introduction.TypeClassInstances.Person

object TypeClassInstances {
  final case class Person(name: String, email: String)
}

object JsonWriterInstances {

  // type class instance
  implicit val stringWriter: JsonWriter[String] =
    new JsonWriter[String] {
      def write(value: String): Json =
        JsString(value)
    }

  // type class instance
  implicit val personWriter: JsonWriter[Person] =
    new JsonWriter[Person] {
      def write(value: Person): Json =
        JsObject(
          Map(
            "name"  -> JsString(value.name),
            "email" -> JsString(value.email)
          )
        )
    }

  implicit val doubleWriter: JsonWriter[Double] = JsNumber(_)

  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
    new JsonWriter[Option[A]] {
      def write(option: Option[A]): Json =
        option match {
          case Some(aValue) => writer.write(aValue)
          case None         => JsNull
        }
    }

  // etc...
}
