package chapter6_semigroupal_and_applicative

import cats.{Apply, Functor, Semigroupal}

object ApplyAndApplicative extends App {

  trait Apply2[F[_]] extends Semigroupal[F] with Functor[F] { // to distinguish from Cats' Apply
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      ap(map(fa)(a => (b: B) => (a, b)))(fb)
  }

  trait Applicative2[F[_]] extends Apply[F] { // to distinguish from Cats' Applicative
    def pure[A](a: A): F[A]
  }
}
