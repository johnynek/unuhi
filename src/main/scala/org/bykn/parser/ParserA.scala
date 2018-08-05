package org.bykn.parser

import cats.{Alternative, Defer, Monad}
import cats.data.NonEmptyList

trait ParserA[P[_]] extends Alternative[P] with Defer[P] {
  def string(str: String): P[Unit]
  def not[A](p: P[A]): P[Unit]
  def oneOfChar(chars: Set[Char]): P[Char]
  def runOption[A](p: P[A], str: String): Option[(String, A)]
  def anyChar: P[Char]

  def char(c: Char): P[Unit] = string(c.toString)

  def oneOf[A](ps: List[P[A]]): P[A] =
    ps match {
      case Nil => failed
      case h :: Nil => h
      case h :: rest => combineK(h, defer(oneOf(rest)))
    }

  def oneOfChars(str: String): P[Char] =
    oneOfChar(str.toSet)

  def failed[A]: P[A] = empty

  // First try the right side (to be right biased)
  def either[A, B](b: P[A], a: P[B]): P[Either[A, B]] =
    combineK(map(a)(Right(_): Either[A, B]), map(b)(Left(_): Either[A, B]))

  def repeated[A](p: P[A]): P[List[A]] = {
    val recurse = defer(repeated(p))

    val atLeastOne = map2(p, recurse) (_ :: _)
    val empty = pure(List.empty[A])
    combineK(atLeastOne, empty)
  }

  def repeated1[A](p: P[A]): P[NonEmptyList[A]] =
    map2(p, repeated(p))(NonEmptyList(_, _))

  def repeated_[A](p: P[A]): P[Unit] = {
    val recurse = defer(repeated_(p))

    val atLeastOne = void(product(p, recurse))
    val empty = unit
    combineK(atLeastOne, empty)
  }

  def repeated1_[A](p: P[A]): P[Unit] =
    void(product(p, repeated_(p)))

  def optional[A](p: P[A]): P[Option[A]] =
    combineK(map(p)(Some(_)), pure(None))

  // 0 or more whitespace1
  val whiteSpace: P[Unit] =
    repeated_(whiteSpace1)

  // Just 1 space or tab
  val whiteSpace1: P[Unit] =
    void(oneOfChar(Set(' ', '\t')))

  val newline1: P[Unit] =
    void(oneOfChar(Set('\n')))

  val digit: P[Char] =
    oneOfChars("0123456789")

  val nonZeroDigit: P[Char] =
    oneOfChars("123456789")
}

object ParserA {
  def apply[P[_]](implicit p: ParserA[P]): ParserA[P] = p

}
