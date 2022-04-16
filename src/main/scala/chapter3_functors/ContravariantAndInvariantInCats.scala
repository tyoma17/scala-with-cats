package chapter3_functors

trait SimpleContravariant[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}

trait SimpleInvariant[F[_]] {
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
}

object ContravariantAndInvariantInCats extends App {

  import cats.Contravariant
  import cats.Show
  import cats.instances.string.catsStdShowForString

  val showString = Show[String]

  val showSymbol = Contravariant[Show].contramap(showString)((sym: Symbol) => s"'${sym.name}")

  showSymbol.show(Symbol("dave"))
  // res1: String = "'dave"

  import cats.syntax.contravariant.toContravariantOps
  showString
    .contramap[Symbol](sym => s"'${sym.name}")
    .show(Symbol("dave"))

  import cats.Monoid
  import cats.instances.string.catsKernelStdMonoidForString // for Monoid
  import cats.syntax.invariant.toInvariantOps // for imap
  import cats.syntax.semigroup.catsSyntaxSemigroup // for |+|

  implicit val symbolMonoid: Monoid[Symbol] =
    Monoid[String].imap(Symbol(_))(_.name)

  Monoid[Symbol].empty
  // res3: Symbol = '

  Symbol("a") |+| Symbol("few") |+| Symbol("words")
  // res4: Symbol = 'afewwords
}
