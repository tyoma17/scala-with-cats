package chapter7_foldable_and_traverse

import cats.Applicative

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

trait Traverse2[F[_]] { // to distinguish with Cats' Traverse
  def traverse[G[_]: Applicative, A, B]
  (inputs: F[A])(func: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, B]
  (inputs: F[G[B]]): G[F[B]] =
    traverse(inputs)(identity)
}

object Traverse2Demo extends App {
  import cats.Traverse
  import cats.instances.future._ // for Applicative
  import cats.instances.list._   // for Traverse

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60) // just for demonstration

  val totalUptime: Future[List[Int]] =
    Traverse[List].traverse(hostnames)(getUptime)

  Await.result(totalUptime, 1.second)
  // res0: List[Int] = List(1020, 960, 840)

  val numbers = List(Future(1), Future(2), Future(3))

  val numbers2: Future[List[Int]] =
    Traverse[List].sequence(numbers)

  Await.result(numbers2, 1.second)
  // res1: List[Int] = List(1, 2, 3)

  import cats.syntax.traverse._ // for sequence and traverse

  Await.result(hostnames.traverse(getUptime), 1.second)
  // res2: List[Int] = List(1020, 960, 840)
  Await.result(numbers.sequence, 1.second)
  // res3: List[Int] = List(1, 2, 3)
}
