package chapter7_foldable_and_traverse

object FoldsAndFolding extends App {

  def show[A](list: List[A]): String =
    list.foldLeft("nil")((accum, item) => s"$item then $accum")

  show(Nil)
  // res0: String = "nil"

  show(List(1, 2, 3))
  // res1: String = "3 then 2 then 1 then nil"

  val res6 = List(1, 2, 3).foldLeft(List[Int]())((acc, next) => next :: acc)
  // res6: List[Int] = List(3, 2, 1)

  val res7 = List(1, 2, 3).foldRight(List[Int]())(_ :: _)
  // res7: List[Int] = List(1, 2, 3)

  def listMapViaFoldRight[A, B](list: List[A])(f: A => B): List[B] =
    list.foldRight(List.empty[B])((next, acc) => f(next) :: acc)

  val res9 = listMapViaFoldRight(List(1, 2, 3))(_ * 2)
  // res9: List[Int] = List(2, 4, 6)

  def listFlatMapViaFoldRight[A, B](list: List[A])(f: A => List[B]): List[B] =
    list.foldRight(List.empty[B])((next, acc) => f(next) ::: acc)

  val res10 = listFlatMapViaFoldRight(List(1, 2, 3))(a => List(a, a * 10, a * 100))
  // res10: List[Int] = List(1, 10, 100, 2, 20, 200, 3, 30, 300)

  def listFilterViaFoldRight[A](list: List[A])(func: A => Boolean): List[A] =
    list.foldRight(List.empty[A]) { (item, accum) =>
      if (func(item)) item :: accum else accum
    }

  listFilterViaFoldRight(List(1, 2, 3))(_ % 2 == 1)
  // res11: List[Int] = List(1, 3)

  def sumWithNumeric[A](list: List[A])(implicit numeric: Numeric[A]): A =
    list.foldRight(numeric.zero)(numeric.plus)

  sumWithNumeric(List(1, 2, 3))
  // res12: Int = 6

  import cats.Monoid

  def sumWithMonoid[A](list: List[A])(implicit monoid: Monoid[A]): A =
    list.foldRight(monoid.empty)(monoid.combine)

  import cats.instances.int._ // for Monoid

  sumWithMonoid(List(1, 2, 3))
  // res13: Int = 6
}
