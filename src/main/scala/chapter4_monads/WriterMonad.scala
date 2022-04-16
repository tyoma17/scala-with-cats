package chapter4_monads

object WriterMonad extends App {

  import cats.data.Writer
  import cats.instances.vector.catsKernelStdMonoidForVector // for Monoid

  Writer(
    Vector(
      "It was the best of times",
      "it was the worst of times"
    ),
    1859
  )
  // res0: cats.data.WriterT[cats.package.Id, Vector[String], Int] = WriterT(
  //   (Vector("It was the best of times", "it was the worst of times"), 1859)
  // )

//  type Writer[W, A] = WriterT[Id, W, A]

  type Logged[A] = Writer[Vector[String], A]
  import cats.syntax.applicative._ // for pure

  123.pure[Logged]
  // res1: Logged[Int] = WriterT((Vector(), 123))

  import cats.syntax.writer.catsSyntaxWriterId // for tell

  Vector("msg1", "msg2", "msg3").tell
  // res2: Writer[Vector[String], Unit] = WriterT(
  //   (Vector("msg1", "msg2", "msg3"), ())
  // )

  val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
  // a: cats.data.WriterT[cats.package.Id, Vector[String], Int] = WriterT(
  //   (Vector("msg1", "msg2", "msg3"), 123)
  // )
  val b = 123.writer(Vector("msg1", "msg2", "msg3"))
  // b: Writer[Vector[String], Int] = WriterT(
  //   (Vector("msg1", "msg2", "msg3"), 123)
  // )

  val aResult: Int = a.value
  // aResult: Int = 123
  val aLog: Vector[String] = a.written
  // aLog: Vector[String] = Vector("msg1", "msg2", "msg3")

  val (log, result) = b.run
  // log: Vector[String] = Vector("msg1", "msg2", "msg3")
  // result: Int = 123

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b
  // writer1: cats.data.WriterT[cats.package.Id, Vector[String], Int] = WriterT(
  //   (Vector("a", "b", "c", "x", "y", "z"), 42)
  // )
  val res3 = writer1.run
  // res3: (Vector[String], Int) = (Vector("a", "b", "c", "x", "y", "z"), 42)

  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
  // writer2: cats.data.WriterT[cats.package.Id, Vector[String], Int] = WriterT(
  //   (Vector("A", "B", "C", "X", "Y", "Z"), 42)
  // )

  val res4 = writer2.run
  // res4: (Vector[String], Int) = (Vector("A", "B", "C", "X", "Y", "Z"), 42)

  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase),
    res => res * 100
  )
  // writer3: cats.data.WriterT[cats.package.Id, Vector[String], Int] = WriterT(
  //   (Vector("A", "B", "C", "X", "Y", "Z"), 4200)
  // )

  val res5 = writer3.run
  // res5: (Vector[String], Int) = (Vector("A", "B", "C", "X", "Y", "Z"), 4200)

  val writer4 = writer1.mapBoth { (log, res) =>
    val log2 = log.map(_ + "!")
    val res2 = res * 1000
    (log2, res2)
  }
  // writer4: cats.data.WriterT[cats.package.Id, Vector[String], Int] = WriterT(
  //   (Vector("a!", "b!", "c!", "x!", "y!", "z!"), 42000)
  // )

  val res6 = writer4.run
  // res6: (Vector[String], Int) = (
  //   Vector("a!", "b!", "c!", "x!", "y!", "z!"),
  //   42000
  // )

  val writer5 = writer1.reset
  // writer5: cats.data.WriterT[cats.package.Id, Vector[String], Int] = WriterT(
  //   (Vector(), 42)
  // )

  val res7 = writer5.run
  // res7: (Vector[String], Int) = (Vector(), 42)

  val writer6 = writer1.swap
  // writer6: cats.data.WriterT[cats.package.Id, Int, Vector[String]] = WriterT(
  //   (42, Vector("a", "b", "c", "x", "y", "z"))
  // )

  val res8 = writer6.run
  // res8: (Int, Vector[String]) = (42, Vector("a", "b", "c", "x", "y", "z"))

  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

//  factorial(5)
  // fact 1 1
  // fact 2 2
  // fact 3 6
  // fact 4 24
  // fact 5 120
  // res9: Int = 120

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.duration._

  Await.result(
    Future.sequence(
      Vector(
        Future(factorial(5)),
        Future(factorial(5))
      )
    ),
    5.seconds
  )
  // fact 0 1
  // fact 0 1
  // fact 1 1
  // fact 1 1
  // fact 2 2
  // fact 2 2
  // fact 3 6
  // fact 3 6
  // fact 4 24
  // fact 4 24
  // fact 5 120
  // fact 5 120
  // res: scala.collection.immutable.Vector[Int] =
  //   Vector(120, 120)

  def factorialLog(n: Int): Logged[Int] =
    for {
      ans <-
        if (n == 0) 1.pure[Logged]
        else slowly(factorialLog(n - 1).map(_ * n))
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  // not via 4 comprehension
  def factorialLogMap(n: Int): Logged[Int] =
    (if (n == 0) 1.pure[Logged]
    else slowly(factorialLog(n - 1).map(_ * n))).flatMap(ans => Vector(s"fact $n $ans").tell.map(_ => ans))

  val (logs, res) = factorialLog(5).run
  // logs: Vector[String] = Vector(
  //   "fact 0 1",
  //   "fact 1 1",
  //   "fact 2 2",
  //   "fact 3 6",
  //   "fact 4 24",
  //   "fact 5 120"
  // )
  // res: Int = 120

  val result2 = Await.result(Future.sequence(Vector(
    Future(factorialLog(5)),
    Future(factorialLog(5))
  )).map(_.map(_.written)), 5.seconds)

  println(result2)
  // res: scala.collection.immutable.Vector[cats.Id[Vector[String]]] =
  //   Vector(
  //     Vector(fact 0 1, fact 1 1, fact 2 2, fact 3 6, fact 4 24, fact 5 120),
  //     Vector(fact 0 1, fact 1 1, fact 2 2, fact 3 6, fact 4 24, fact 5 120)
  //   )
}
