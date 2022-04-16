package chapter6_semigroupal_and_applicative

trait Semigroupal2[F[_]] { // to distinguish from Cats' Semigroupal
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

object Demo extends App {

  import cats.Semigroupal
  import cats.instances.option._ // for Semigroupal
  val res1 = Semigroupal[Option].product(Some(123), Some("abc"))
  // res1: Option[(Int, String)] = Some((123, "abc"))
  val res2 = Semigroupal[Option].product(None, Some("abc"))
  // res2: Option[Tuple2[Nothing, String]] = None
  val res3 = Semigroupal[Option].product(Some(123), None)
  // res3: Option[Tuple2[Int, Nothing]] = None

  val res4 = Semigroupal.tuple3(Option(1), Option(2), Option(3))
  // res4: Option[(Int, Int, Int)] = Some((1, 2, 3))

  val res5 = Semigroupal.tuple3(Option(1), Option(2), None)
  // res5: Option[(Int, Int, Int)] = None

  Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
  // res6: Option[Int] = Some(6)

  Semigroupal.map2(Option(1), Option.empty[Int])(_ + _)
  // res7: Option[Int] = None

  // Apply Syntax
  import cats.syntax.apply._
  (Option(123), Option("abc")).tupled
  // res8: Option[(Int, String)] = Some((123, "abc"))

  (Option(123), Option("abc"), Option(true)).tupled
  // res9: Option[(Int, String, Boolean)] = Some((123, "abc", true))

  final case class Cat(name: String, born: Int, color: String)

  val res10 = (
    Option("Garfield"),
    Option(1978),
    Option("Orange & black")
  ).mapN(Cat)
  // res10: Option[Cat] = Some(Cat("Garfield", 1978, "Orange & black"))

  // 6.2.1 Fancy Functors and Apply Syntax

  import cats.Monoid
  import cats.instances.int._       // for Monoid
  import cats.instances.invariant._ // for Semigroupal
  import cats.instances.list._      // for Monoid
  import cats.instances.string._    // for Monoid

  final case class Cat2(
      name: String,
      yearOfBirth: Int,
      favoriteFoods: List[String]
  )

  val tupleToCat: (String, Int, List[String]) => Cat2 =
    Cat2

  val catToTuple: Cat2 => (String, Int, List[String]) =
    cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

  implicit val catMonoid: Monoid[Cat2] = (
    Monoid[String],
    Monoid[Int],
    Monoid[List[String]]
  ).imapN(tupleToCat)(catToTuple)

  val garfield   = Cat2("Garfield", 1978, List("Lasagne"))
  val heathcliff = Cat2("Heathcliff", 1988, List("Junk Food"))
  import cats.syntax.semigroup._
  val res14 = garfield |+| heathcliff
  // res14: Cat = Cat("GarfieldHeathcliff", 3966, List("Lasagne", "Junk Food"))

  // Semigroupal Applied to Different Types

  import cats.instances.future._ // for Semigroupal
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123))

  Await.result(futurePair, 1.second)
  // res0: (String, Int) = ("Hello", 123)

  val futureCat = (
    Future("Garfield"),
    Future(1978),
    Future(List("Lasagne"))
  ).mapN(Cat2)

  Await.result(futureCat, 1.second)
  // res1: Cat = Cat("Garfield", 1978, List("Lasagne"))

  Semigroupal[List].product(List(1, 2), List(3, 4))
  // res2: List[(Int, Int)] = List((1, 3), (1, 4), (2, 3), (2, 4))

  type ErrorOr[A] = Either[Vector[String], A]
  import cats.instances.either._
  Semigroupal[ErrorOr].product(
    Left(Vector("Error 1")),
    Left(Vector("Error 2"))
  )
  // res3: ErrorOr[Tuple2[Nothing, Nothing]] = Left(Vector("Error 1"))

  // Semigroupal Applied to Monads
  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatmap

  def product[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
    for {
      a <- fa
      b <- fb
    } yield (a, b)
  }

}
