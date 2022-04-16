package chapter7_foldable_and_traverse

object FoldableInCats extends App {

  import cats.Foldable
  import cats.instances.list._ // for Foldable

  val ints = List(1, 2, 3)

  Foldable[List].foldLeft(ints, 0)(_ + _)
  // res0: Int = 6

  import cats.instances.option._ // for Foldable

  val maybeInt = Option(123)

  Foldable[Option].foldLeft(maybeInt, 10)(_ * _)
  // res1: Int = 1230

  // Folding right
//  def foldRight[A, B](fa: F[A], lb: Eval[B])
//                     (f: (A, Eval[B]) => Eval[B]): Eval[B]

  import cats.Eval
  import cats.Foldable

  def bigData = (1 to 100_000).to(Stream)

  println(bigData.foldRight(0L)(_ + _))
  // java.lang.StackOverflowError ... ACTUALLY not (my guess: in previous Scala version it wasn't stack safe)

  import cats.instances.stream._ // for Foldable

  val eval: Eval[Long] =
    Foldable[Stream].foldRight(bigData, Eval.now(0L)) { (num, eval) =>
      eval.map(_ + num)
    }

  eval.value
  // res3: Long = 5000050000L

  // Folding with Monoids
  Foldable[Option].nonEmpty(Option(42))
  // res6: Boolean = true

  Foldable[List].find(List(1, 2, 3))(_ % 2 == 0)
  // res7: Option[Int] = Some(2)

  import cats.instances.int._ // for Monoid

  Foldable[List].combineAll(List(1, 2, 3))
  // res8: Int = 6

  import cats.instances.string._ // for Monoid

  Foldable[List].foldMap(List(1, 2, 3))(_.toString)
  // res9: String = "123"

  import cats.instances.vector._ // for Monoid

  val ints2 = List(Vector(1, 2, 3), Vector(4, 5, 6))

  (Foldable[List] compose Foldable[Vector]).combineAll(ints2)
  // res11: Int = 21

  // Syntax for Foldable
  import cats.syntax.foldable._ // for combineAll and foldMap

  List(1, 2, 3).combineAll
  // res12: Int = 6

  List(1, 2, 3).foldMap(_.toString)
  // res13: String = "123"
}
