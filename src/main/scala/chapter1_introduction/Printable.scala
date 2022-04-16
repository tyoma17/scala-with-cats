package chapter1_introduction

trait Printable[A] {
  def format(a: A): String
  def contramap[B](func: B => A): Printable[B] =
    b => format(func(b))
}

object Printable {
  def format[A](a: A)(implicit printable: Printable[A]): String = printable.format(a)
  def print[A](a: A)(implicit printable: Printable[A]): Unit    = println(format(a))
}

object PrintableInstances {
  implicit val printableString: Printable[String] = identity
  implicit val printableInt: Printable[Int]       = _.toString
  implicit val printableCat: Printable[Cat]       = cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
}

object PrintableSyntax {
  implicit class PrintableOps[A](a: A) {
    def format(implicit printable: Printable[A]): String = printable.format(a)
    def print(implicit printable: Printable[A]): Unit = println(format)
  }
}

final case class Cat(name: String, age: Int, color: String)

object Demo extends App {
  import PrintableInstances.printableString
  Printable.format("a string")
  // a string
  Printable.print("a string")
  // ()
  // a string

  val cat = Cat("Barsik", 6, "ginger")
  import PrintableInstances.printableCat
  Printable.print(cat)
  import PrintableSyntax.PrintableOps
  cat.format
}
