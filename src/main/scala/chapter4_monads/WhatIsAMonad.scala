package chapter4_monads

object WhatIsAMonad extends App {

  def parseInt(str: String): Option[Int] =
    scala.util.Try(str.toInt).toOption

  def divide(a: Int, b: Int): Option[Int] =
    if(b == 0) None else Some(a / b)

//  def stringDivideBy(aStr: String, bStr: String): Option[Int] =
//    parseInt(aStr).flatMap { aNum =>
//      parseInt(bStr).flatMap { bNum =>
//        divide(aNum, bNum)
//      }
//    }

  def stringDivideBy(aStr: String, bStr: String): Option[Int] =
    for {
      aNum <- parseInt(aStr)
      bNum <- parseInt(bStr)
      ans  <- divide(aNum, bNum)
    } yield ans

  stringDivideBy("6", "2")
  // res0: Option[Int] = Some(3)
  stringDivideBy("6", "0")
  // res1: Option[Int] = None
  stringDivideBy("6", "foo")
  // res2: Option[Int] = None
  stringDivideBy("bar", "2")
  // res3: Option[Int] = None

  // Lists
  for {
    x <- (1 to 3).toList
    y <- (4 to 5).toList
  } yield (x, y)
  // res5: List[(Int, Int)] = List(
  //   (1, 4),
  //   (1, 5),
  //   (2, 4),
  //   (2, 5),
  //   (3, 4),
  //   (3, 5)
  // )

  // Futures
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  def doSomethingLongRunning: Future[Int] = ???
  def doSomethingElseLongRunning: Future[Int] = ???

  def doSomethingVeryLongRunning: Future[Int] =
    for {
      result1 <- doSomethingLongRunning
      result2 <- doSomethingElseLongRunning
    } yield result1 + result2
}
