package chapter3_functors

object FunctorsInCats extends App {

  import cats.Functor
  import cats.instances.list.catsStdInstancesForList
  val list1 = List(1, 2, 3)
  // list1: List[Int] = List(1, 2, 3)
  val list2 = Functor[List].map(list1)(_ * 2)
  // list2: List[Int] = List(2, 4, 6)

  import cats.instances.option.catsStdInstancesForOption
  val option1 = Option(123)
  // option1: Option[Int] = Some(123)
  val option2 = Functor[Option].map(option1)(_.toString)
  // option2: Option[String] = Some("123")

  // lift converts a function of type A => B to one that operates over a functor and has type F[A] => F[B]
  val intToString                               = (x: Int) => x.toString
  val liftedFunc: Option[Int] => Option[String] = Functor[Option].lift(intToString)
  liftedFunc(Option(1))
  // res1: Option[String] = Some("1")

  Functor[List].as(list1, "As")
  // res2: List[String] = List("As", "As", "As")

  // functor syntax
  import cats.syntax.functor.toFunctorOps
  import cats.instances.function.catsStdMonadForFunction1
  val func1 = (a: Int) => a + 1
  val func2 = (a: Int) => a * 2
  val func3 = (a: Int) => s"$a!"
  val func4 = func1.map(func2).map(func3)

  func4(123)
  // res3: String = "248!"

  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
    start.map(n => n + 2)

  doMath(Option(20))
  // res4: Option[Int] = Some(22)
  doMath(List(1, 2, 3))
  // res5: List[Int] = List(3, 4, 5)

  list1.as("As")
  // res7: List[String] = List("As", "As", "As")

}
