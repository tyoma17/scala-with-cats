package chapter1_introduction

import cats.Show
import cats.implicits.toShow

object DefiningCustomInstances extends App {

  import java.util.Date

//  implicit val dateShow: Show[Date] =
//    new Show[Date] {
//      def show(date: Date): String =
//        s"${date.getTime}ms since the epoch."
//    }

  implicit val dateShow: Show[Date] = Show.show(date => s"${date.getTime}ms since the epoch.")

  new Date().show
  // res1: String = "1594215596646ms since the epoch."

  implicit val stringShow: Show[String] = Show.fromToString[String]
  "A string".show

  implicit val catShow: Show[Cat] = Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat.")
  val cat = Cat("Barsik", 6, "ginger")
  cat.show
  // Barsik is a 6 year-old ginger cat.
}
