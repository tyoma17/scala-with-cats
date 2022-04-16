package chapter3_functors

import chapter1_introduction.Printable

object ContravariantMap extends App {

  implicit val stringPrintable: Printable[String] =
    (value: String) => s"'$value'"

  implicit val booleanPrintable: Printable[Boolean] =
    (value: Boolean) => if (value) "yes" else "no"

  import Printable.format
  format("hello")
  // res2: String = "'hello'"
  format(true)
  // res3: String = "yes"

  final case class Box[A](value: A)

  implicit def boxPrintable[A](implicit printable: Printable[A]): Printable[Box[A]] =
    printable.contramap(_.value)

  format(Box("hello world"))
  // res4: String = "'hello world'"
  format(Box(true))
  // res5: String = "yes"
}
