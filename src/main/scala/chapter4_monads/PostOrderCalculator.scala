package chapter4_monads

object PostOrderCalculator extends App {
  import cats.data.State
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)

      case _ =>
        sys.error("Fail!")
    }

  evalOne("42").runA(Nil).value
  // res10: Int = 42

  val program = for {
    _   <- evalOne("1")
    _   <- evalOne("2")
    ans <- evalOne("+")
  } yield ans
  // program: cats.data.IndexedStateT[cats.Eval, List[Int], List[Int], Int] = cats.data.IndexedStateT@3afcc7dd

  program.runA(Nil).value
  // res11: Int = 3

  import cats.syntax.applicative._
  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState])((state, string) => state.flatMap(_ => evalOne(string)))

  val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))
  // multistageProgram: CalcState[Int] = cats.data.IndexedStateT@228a6340

  multistageProgram.runA(Nil).value
  // res13: Int = 9

  val biggerProgram = for {
    _   <- evalAll(List("1", "2", "+"))
    _   <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans
  // biggerProgram: cats.data.IndexedStateT[cats.Eval, List[Int], List[Int], Int] = cats.data.IndexedStateT@1a227435

  biggerProgram.runA(Nil).value
  // res14: Int = 21

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  evalInput("1 2 + 3 4 + *")
  // res15: Int = 21
}
