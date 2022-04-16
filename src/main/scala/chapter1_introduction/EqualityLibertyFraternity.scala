package chapter1_introduction

object EqualityLibertyFraternity extends App {

  import cats.Eq
  import cats.instances.int.catsKernelStdOrderForInt

  val eqInt = Eq[Int]

  eqInt.eqv(123, 123)
  // res1: Boolean = true
  eqInt.eqv(123, 234)
  // res2: Boolean = false

  // eqInt.eqv(123, "234")
  // error: type mismatch;
  // found   : String("234")
  // required: Int

  import cats.syntax.eq.catsSyntaxEq // for === and =!=
  123 === 123
  // res4: Boolean = true
  123 =!= 234
  // res5: Boolean = true

  // 123 === "123"
  // error: type mismatch;
  //  found   : String("123")
  //  required: Int
  // 123 === "123"
  //         ^^^^^

  import cats.instances.option.catsKernelStdEqForOption
//  Some(1) === None
//  Some(1) === None
  // error: value === is not a member of Some[Int]
  // Some(1) === None
  // ^^^^^^^^^^^

  (Some(1): Option[Int]) === (None: Option[Int])
  // res8: Boolean = false

  Option(1) === Option.empty[Int]
  // res9: Boolean = false

  import cats.syntax.option.catsSyntaxOptionId
  import cats.syntax.option.none

  1.some === none[Int]
  // res10: Boolean = false
  1.some =!= none[Int]
  // res11: Boolean = true

  // Comparing custom types

  import java.util.Date
  val x = new Date() // now
  val y = new Date() // a bit later than now
  import cats.instances.long.catsKernelStdOrderForLong
  implicit val dateEq: Eq[Date] = Eq.instance((d1, d2) => d1.getTime === d2.getTime)

  x === x
  // res12: Boolean = true
  x === y
  // res13: Boolean = false

  val cat1 = Cat("Garfield",   38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  implicit val catEq: Eq[Cat] = Eq.instance((c1, c2) => {
    import cats.instances.string.catsKernelStdOrderForString
    c1.color === c2.color && c1.age === c2.age && c1.name === c2.name
  })

  cat1 === cat2
}
