package org.bykn.unuhi

import cats.Monad

trait ParserM[P[_]] extends ParserA[P] with Monad[P] {

  override def repeated[A](p: P[A]): P[List[A]] =
    map(tailRecM(List.empty[A]) { as =>
      // somehow 2.10 needs the type annotation here
      map[Either[List[A], List[A]], Either[List[A], List[A]]](either(pure(as), map(p)(_ :: as)))(_.swap)
    })(_.reverse)

  override def repeated_[A](p: P[A]): P[Unit] =
    tailRecM(()) { _ =>
      // see if we can get one more:
      map(either(unit, void(p)))(_.swap)
    }
}

object ParserM {
  def apply[P[_]](implicit parser: ParserM[P]): ParserM[P] = parser
}
