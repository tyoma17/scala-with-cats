package chapter5_monad_transformers

import cats.Monad

object ComposingMonads {
  import cats.syntax.applicative._ // for pure

  // Hypothetical example. This won't actually compile:
  def compose[M1[_]: Monad, M2[_]: Monad] = {
    type Composed[A] = M1[M2[A]]

    new Monad[Composed] {
      def pure[A](a: A): Composed[A] =
        a.pure[M2].pure[M1]

      def flatMap[A, B](fa: Composed[A])(f: A => Composed[B]): Composed[B] =
        ???
//      fa.flatMap(_.fold[Composed[B]](None.pure[M1])(f))

      override def tailRecM[A, B](a: A)(f: A => Composed[Either[A, B]]): Composed[B] = ???
    }
  }
}
