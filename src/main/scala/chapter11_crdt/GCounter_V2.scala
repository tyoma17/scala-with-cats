package chapter11_crdt

import cats.instances.list._
import cats.instances.map._
import cats.kernel.CommutativeMonoid
import cats.syntax.semigroup._
import cats.syntax.foldable._ // for combineAll

final case class GCounter_V2[A](counters: Map[String, A]) {

  def increment(machine: String, amount: A)(implicit m: CommutativeMonoid[A]): GCounter_V2[A] = {
    val value = amount |+| counters.getOrElse(machine, m.empty)
    GCounter_V2(counters + (machine -> value))
  }

  def merge(that: GCounter_V2[A])(implicit b: BoundedSemiLattice[A]): GCounter_V2[A] =
    GCounter_V2(this.counters |+| that.counters)

  def total(implicit m: CommutativeMonoid[A]): A =
    this.counters.values.toList.combineAll
}
