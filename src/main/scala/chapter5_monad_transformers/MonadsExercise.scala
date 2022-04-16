package chapter5_monad_transformers

import cats.data.EitherT
import cats.implicits.catsStdInstancesForFuture

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

object MonadsExercise extends App {

//  type Response[A] = Future[Either[String, A]]
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  import cats.syntax.applicative._
  import scala.concurrent.ExecutionContext.Implicits.global
  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(value) => value.pure[Response]
      case None        => EitherT.left(Future(s"$autobot unreachable"))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      ally1Level <- getPowerLevel(ally1)
      ally2Level <- getPowerLevel(ally2)
    } yield (ally1Level + ally2Level) > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val future = canSpecialMove(ally1, ally2)
      .fold(identity, if (_) s"$ally1 and $ally2 are ready to roll out!" else s"$ally1 and $ally2 need a recharge.")
    Await.result(future, 1 second)
  }

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Bumblebee", "Hot Rod"))
  println(tacticalReport("Jazz", "Ironhide"))

}
