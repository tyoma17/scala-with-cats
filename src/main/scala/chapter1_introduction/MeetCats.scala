package chapter1_introduction

object MeetCats extends App {

  import cats.Show
  import cats.instances.int.catsStdShowForInt
  val showInt = Show.apply[Int]

  val intAsString: String =
    showInt.show(123)
  // intAsString: String = "123"

  import cats.instances.string.catsStdShowForString
  val showString: Show[String] = Show.apply[String]

  val stringAsString: String =
    showString.show("abc")
  // stringAsString: String = "abc"

  import cats.syntax.show.toShow
  val shownInt = 123.show
  // shownInt: String = "123"

  val shownString = "abc".show
  // shownString: String = "abc"

  import cats._ // imports all of Cats’ type classes in one go
  import cats.implicits._ // imports all of the standard type class instances and all of the syntax in one go.
}
