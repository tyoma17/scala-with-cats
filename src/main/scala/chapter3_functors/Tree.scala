package chapter3_functors

import chapter3_functors.Tree.leaf

sealed trait Tree[+A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)
}

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Demo extends App {
  import cats.Functor
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value)         => Leaf(f(value))
    }
  }

  val branch = Tree.branch(
    left  = Tree.branch(leaf(2), leaf(3)),
    right = leaf(10)
  )

  import cats.syntax.functor.toFunctorOps
  branch.map(_ * 10)
  // Branch(Branch(Leaf(20),Leaf(30)),Leaf(100))
}
