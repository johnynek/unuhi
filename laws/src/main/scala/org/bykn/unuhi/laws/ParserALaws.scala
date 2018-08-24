package org.bykn.unuhi.laws

import org.bykn.unuhi._

import cats.laws.{DeferLaws, AlternativeLaws, IsEq}

import cats.implicits._

/**
 * Laws that must be obeyed by any `Monad`.
 */
trait ParserALaws[F[_]] extends AlternativeLaws[F] with DeferLaws[F] {
  implicit override def F: ParserA[F]
}

object ParserALaws {
  def apply[F[_]](implicit ev: ParserA[F]): ParserALaws[F] =
    new ParserALaws[F] { def F: ParserA[F] = ev }
}
