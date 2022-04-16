package chapter4_monads

object EvalMonad extends App {

  val x = {
    println("Computing X")
    math.random
  }
  // Computing X
  // x: Double = 0.15241729989551633

  val res0 = x // first access
  // res0: Double = 0.15241729989551633 // first access
  val res1 = x // second access
  // res1: Double = 0.15241729989551633

  def y = {
    println("Computing Y")
    math.random
  }

  y // first access
  // Computing Y
  // res2: Double = 0.6963618800921411 // first access
  y // second access
  // Computing Y
  // res3: Double = 0.7321640587866993

  lazy val z = {
    println("Computing Z")
    math.random
  }

  z // first access
  // Computing Z
  // res4: Double = 0.18457255119783122 // first access
  z // second access
  // res5: Double = 0.18457255119783122

  import cats.Eval

  val now = Eval.now(math.random + 1000)
  // now: Eval[Double] = Now(1000.020590704322)
  val always = Eval.always(math.random + 3000)
  // always: Eval[Double] = cats.Always@4d8ca6eb
  val later = Eval.later(math.random + 2000)
  // later: Eval[Double] = cats.Later@601dc0b2

  now.value
  // res6: Double = 1000.020590704322
  always.value
  // res7: Double = 3000.97102818157
  later.value
  // res8: Double = 2000.0126977436273

  val greeting = Eval
    .always {
      println("Step 1"); "Hello"
    }
    .map { str => println("Step 2"); s"$str world" }
  // greeting: Eval[String] = cats.Eval$$anon$4@2319703e

  greeting.value
  // Step 1
  // Step 2
  // res16: String = "Hello world"

  val ans = for {
    a <- Eval.now {
      println("Calculating A"); 40
    }
    b <- Eval.always {
      println("Calculating B"); 2
    }
  } yield {
    println("Adding A and B")
    a + b
  }
  // Calculating A
  // ans: Eval[Int] = cats.Eval$$anon$4@2d0f2cbf

  ans.value // first access
  // Calculating B
  // Adding A and B
  // res17: Int = 42 // first access
  ans.value // second access
  // Calculating B
  // Adding A and B
  // res18: Int = 42

  val saying = Eval
    .always {
      println("Step 1"); "The cat"
    }
    .map { str => println("Step 2"); s"$str sat on" }
    .memoize
    .map { str => println("Step 3"); s"$str the mat" }
  // saying: Eval[String] = cats.Eval$$anon$4@ca01c64

  saying.value // first access
  // Step 1
  // Step 2
  // Step 3
  // res19: String = "The cat sat on the mat" // first access
  saying.value // second access
  // Step 3
  // res20: String = "The cat sat on the mat"

  // Trampolining and Eval.defer
  //  def factorial(n: BigInt): BigInt =
  //    if(n == 1) n else n * factorial(n - 1)

  // factorial(50000)
  // java.lang.StackOverflowError
  //   ...

//  def factorial(n: BigInt): Eval[BigInt] =
//    if (n == 1) Eval.now(n)
//    else factorial(n - 1).map(_ * n)
//
//  factorial(50000).value
//  // java.lang.StackOverflowError
//  //   ...

  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }

  factorial(50000).value
  // res: A very big value

  def foldRight_notStackSafe[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight_notStackSafe(tail, acc)(fn))
      case Nil =>
        acc
    }

  val list = (1 to 50_000).toList

//  foldRight_notStackSafe(list, 0)(_ + _)

  def foldRight_mySolution[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(foldRight_mySolution(tail, acc)(fn)).map(fn(head, _))
      case Nil =>
        Eval.now(acc)
    }
  foldRight_mySolution(list, 0)(_ + _).value

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {

    def foldRightEval(as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
      as match {
        case head :: tail =>
          Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
        case Nil =>
          acc
      }

    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value
  }

  foldRight((1 to 100000).toList, 0L)(_ + _)
  // res24: Long = 5000050000L
}
