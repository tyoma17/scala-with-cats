package chapter4_monads

import cats.Monad

object MonadErrorTypeClass {
  trait MonadError[F[_], E] extends Monad[F] {
    // Lift an error into the `F` context:
    def raiseError[A](e: E): F[A]

    // Handle an error, potentially recovering from it:
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

    // Handle all errors, recovering from them:
    def handleError[A](fa: F[A])(f: E => A): F[A]

    // Test an instance of `F`,
    // failing if the predicate is not satisfied:
    def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
  }
}
