package chapter9_map_reduce

import scala.language.postfixOps

object FoldMap extends App {
  import cats.Monoid

  /** Single-threaded map-reduce function.
    * Maps `func` over `values` and reduces using a `Monoid[B]`.
    */
  import cats.instances.vector._
  import cats.syntax.foldable._

  def foldMap[A, B: Monoid](values: Vector[A])(func: A => B): B =
    values.map(func).combineAll

  import cats.instances.int._ // for Monoid
  foldMap(Vector(1, 2, 3))(identity)

  import cats.instances.string._ // for Monoid
  foldMap(Vector(1, 2, 3))(_.toString + "! ")
  // res2: String = "1! 2! 3! "

  foldMap("Hello world!".toVector)(_.toString.toUpperCase)
  // res3: String = "HELLO WORLD!"

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  val future1 = Future {
    (1 to 100).toList.foldLeft(0)(_ + _)
  }
  // future1: Future[Int] = Future(Success(5050))

  val future2 = Future {
    (100 to 200).toList.foldLeft(0)(_ + _)
  }
  // future2: Future[Int] = Future(Success(15150))

  val future3 = future1.map(_.toString)
  // future3: Future[String] = Future(Success(5050))

  val future4 = for {
    a <- future1
    b <- future2
  } yield a + b
  // future4: Future[Int] = Future(Success(20200))

  Future.sequence(List(Future(1), Future(2), Future(3)))
  // res6: Future[List[Int]] = Future(Success(List(1, 2, 3)))

  import cats.instances.future._ // for Applicative
  import cats.instances.list._   // for Traverse
  import cats.syntax.traverse._  // for sequence

  List(Future(1), Future(2), Future(3)).sequence
  // res7: Future[List[Int]] = Future(Success(List(1, 2, 3)))

  import scala.concurrent._
  import scala.concurrent.duration._

  Await.result(Future(1), 1.second) // wait for the result
  // res8: Int = 1

  import cats.Monad
  Monad[Future].pure(42)
  Monoid[Future[Int]].combine(Future(1), Future(2))

  val coreNum = Runtime.getRuntime.availableProcessors
  // coreNum: Int = 16

  (1 to 10).toList.grouped(3).toList
  // res12: List[List[Int]] = List(
  //   List(1, 2, 3),
  //   List(4, 5, 6),
  //   List(7, 8, 9),
  //   List(10)
  // )

  import cats.syntax.monoid._
  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val groupSize = (1.0 * values.size / coreNum).ceil.toInt
    val futures = values.grouped(groupSize).map(group => Future(foldMap(group)(func)))
    Future.sequence(futures).map(_.foldLeft(Monoid[B].empty)(_ |+| _))
  }

  val result: Future[Int] =
    parallelFoldMap((1 to 1_000_000).toVector)(identity)

  val res14 = Await.result(result, 1 second)
  // res14: Int = 1784293664

  def parallelFoldMap_V2[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores  = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt

    values
      .grouped(groupSize)
      .toVector
      .traverse(group => Future(group.foldMap(func)))
      .map(_.combineAll)
  }

  val future: Future[Int] =
    parallelFoldMap_V2((1 to 1000).toVector)(_ * 1000)

  val res18 = Await.result(future, 1.second)
  // res18: Int = 500500000
}
