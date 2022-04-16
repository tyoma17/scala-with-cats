package chapter4_monads

object StateMonad extends App {

  import cats.data.State

  val a = State[Int, String] { state =>
    (state, s"The state is $state")
  }

  // Get the state and the result:
  val (state, result) = a.run(10).value
  // state: Int = 10
  // result: String = "The state is 10"

  // Get the state, ignore the result:
  val justTheState = a.runS(10).value
  // justTheState: Int = 10

  // Get the result, ignore the state:
  val justTheResult = a.runA(10).value
  // justTheResult: String = "The state is 10"

  // Composing and Transforming State

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both: State[Int, (String, String)] = for {
    a <- step1
    b <- step2
  } yield (a, b)

  val (state2, result2) = both.run(20).value
  // state: Int = 42
  // result: (String, String) = ("Result of step1: 21", "Result of step2: 42")

  val getDemo = State.get[Int]
  // getDemo: State[Int, Int] = cats.data.IndexedStateT@741518c8
  getDemo.run(10).value
  // res1: (Int, Int) = (10, 10)

  val setDemo = State.set[Int](30)
  // setDemo: State[Int, Unit] = cats.data.IndexedStateT@509fb0a
  setDemo.run(10).value
  // res2: (Int, Unit) = (30, ())

  val pureDemo = State.pure[Int, String]("Result")
  // pureDemo: State[Int, String] = cats.data.IndexedStateT@562ae0a8
  pureDemo.run(10).value
  // res3: (Int, String) = (10, "Result")

  val inspectDemo = State.inspect[Int, String](x => s"$x!")
  // inspectDemo: State[Int, String] = cats.data.IndexedStateT@2dc6b50f
  inspectDemo.run(10).value
  // res4: (Int, String) = (10, "10!")

  val modifyDemo = State.modify[Int](_ + 1)
  // modifyDemo: State[Int, Unit] = cats.data.IndexedStateT@71c93b27
  modifyDemo.run(10).value
  // res5: (Int, Unit) = (11, ())

  import State._
  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)
  // program: State[Int, (Int, Int, Int)] = cats.data.IndexedStateT@3b525fbf

  val (state3, result3) = program.run(1).value
  // state: Int = 3
  // result: (Int, Int, Int) = (1, 2, 3000)
}
