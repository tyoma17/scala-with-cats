package chapter7_foldable_and_traverse

object TraverseDemo extends App {

  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60) // just for demonstration

  val allUptimes: Future[List[Int]] =
    hostnames.foldLeft(Future(List.empty[Int])) { (accum, host) =>
      val uptime = getUptime(host)
      for {
        accum  <- accum
        uptime <- uptime
      } yield accum :+ uptime
    }

  Await.result(allUptimes, 1.second)
  // res0: List[Int] = List(1020, 960, 840)

  val allUptimes2: Future[List[Int]] =
    Future.traverse(hostnames)(getUptime)

  Await.result(allUptimes2, 1.second)
  // res2: List[Int] = List(1020, 960, 840)

//  Traversing with Applicatives
  Future(List.empty[Int])
  // is equivalent to
  import cats.instances.future._   // for Applicative
  import cats.syntax.applicative._ // for pure

  List.empty[Int].pure[Future]

  def oldCombine(
      accum: Future[List[Int]],
      host: String
  ): Future[List[Int]] = {
    val uptime = getUptime(host)
    for {
      accum  <- accum
      uptime <- uptime
    } yield accum :+ uptime
  }

  import cats.syntax.apply._ // for mapN

  // Combining accumulator and hostname using an Applicative:
  def newCombine(accum: Future[List[Int]],
                 host: String): Future[List[Int]] =
    (accum, getUptime(host)).mapN(_ :+ _)

  import cats.Applicative

  def listTraverse[F[_]: Applicative, A, B]
  (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B]
  (list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  val totalUptime = listTraverse(hostnames)(getUptime)

  Await.result(totalUptime, 1.second)
  // res5: List[Int] = List(1020, 960, 840)

  import cats.instances.vector._
  listSequence(List(Vector(1, 2), Vector(3, 4)))
  // res7: Vector[List[Int]] = Vector(
  //   List(1, 3),
  //   List(1, 4),
  //   List(2, 3),
  //   List(2, 4)
  // )

  listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
  // res9: Vector[List[Int]] = Vector(
  //   List(1, 3, 5),
  //   List(1, 3, 6),
  //   List(1, 4, 5),
  //   List(1, 4, 6),
  //   List(2, 3, 5),
  //   List(2, 3, 6),
  //   List(2, 4, 5),
  //   List(2, 4, 6)
  // )

  import cats.instances.option._ // for Applicative

  def process(inputs: List[Int]) =
    listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)

  process(List(2, 4, 6))
  // res12: Option[List[Int]] = Some(List(2, 4, 6))
  process(List(1, 2, 3))
  // res13: Option[List[Int]] = None

  import cats.data.Validated
  import cats.instances.list._ // for Monoid

  type ErrorsOr[A] = Validated[List[String], A]

  def process2(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if(n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }

  process2(List(2, 4, 6))
  // res17: ErrorsOr[List[Int]] = Valid(List(2, 4, 6))
  process2(List(1, 2, 3))
  // res18: ErrorsOr[List[Int]] = Invalid(List("1 is not even", "3 is not even"))
}
