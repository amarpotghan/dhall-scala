package dhall.utilities

object TypeLevelFunctions {
  trait Partial2[F[_, _], A] {
    type Apply[B] = F[A, B]
  }
}

