package org.bykn.unuhi.fastparse

import org.bykn.unuhi._
import cats.data.NonEmptyList
import _root_.fastparse.all._

object FastparseParser {
  implicit val fastParseParserA: ParserA[P] =
    new ParserA[P] {
      def anyChar = AnyChar.!.map(_.charAt(0))
      override def unit = Pass
      def defer[A](p: => P[A]): P[A] = P(p)
      def pure[A](a: A) = PassWith(a)
      override def product[A, B](a: P[A], b: P[B]) = a ~ b
      override def oneOf[A](ps: List[P[A]]) =
        ps match {
          case Nil => Fail
          case h :: Nil => h
          case h :: tail => h | P(oneOf(tail))
        }
      def combineK[A](p1: P[A], p2: P[A]) = p1 | p2
      def string(str: String) = P(str)
      def runOption[A](p: P[A], str: String) =
        p.parse(str) match {
          case Parsed.Success(a, chars) => Some((str.substring(chars), a))
          case _ => None
        }
      def empty[A] = Fail

      def not[A](p: P[A]) = !p
      def oneOfChar(cs: Set[Char]) = CharIn(cs.toArray).!.map(_.charAt(0))

      override def map[A, B](p: P[A])(fn: A => B): P[B] =
        p.map(fn)

      def ap[A, B](p: P[A => B])(pa: P[A]): P[B] =
        map(product(p, pa)) { case (fn, a) => fn(a) }

      override def widen[A, B >: A](p: P[A]): P[B] = p

      override def optional[A](p: P[A]): P[Option[A]] = p.?
      override def repeated[A](p: P[A]): P[List[A]] = p.rep().map(_.toList)
      override def repeated1[A](p: P[A]): P[NonEmptyList[A]] =
        p.rep(1).map { as => NonEmptyList(as.head, as.tail.toList) }
    }
}
