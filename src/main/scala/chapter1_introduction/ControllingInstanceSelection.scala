package chapter1_introduction

import chapter1_introduction.TheTypeClass.{Json, JsonWriter}

object ControllingInstanceSelection extends App {

  trait F[+A] // the "+" means "covariant"

  sealed trait Shape
  case class Circle(radius: Double) extends Shape
  val circles: List[Circle] = List(Circle(1))
  val shapes: List[Shape] = circles

  trait G[-A] // the "-" means "contravariant"
  // contravariance means that the type F[B] is a subtype of F[A] if A is a subtype of B

  val shape: Shape = ???
  val circle: Circle = ???

  val shapeWriter: JsonWriter[Shape] = ???
  val circleWriter: JsonWriter[Circle] = ???

  def format[A](value: A, writer: JsonWriter[A]): Json =
    writer.write(value)

  format(shape, shapeWriter)
  format(circle, circleWriter)
  // JsonWriter[Shape] is subtype of JsonWriter[Circle]
  format(circle, shapeWriter)
  // format(shape, circleWriter) // will not compile

  trait H[A] // invariance
//  This means the types F[A] and F[B] are never subtypes of one another
}
