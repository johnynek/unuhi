package org.bykn.unuhi.laws

import org.bykn.unuhi._

import cats.laws.{DeferLaws, AlternativeLaws, IsEq, IsEqArrow}
import cats.implicits._

/**
 * Laws that must be obeyed by any `ParserA`.
 */
trait ParserALaws[F[_]] extends AlternativeLaws[F] with DeferLaws[F] {
  implicit override def F: ParserA[F]

  private val referenceF: ParserA[F] =
    new ParserA[F] {
      def char(c: Char): F[Unit] = F.char(c)
      def pure[A](a: A): F[A] = F.pure(a)
      def ap[A, B](fab: F[A => B])(fa: F[A]): F[B] = F.ap(fab)(fa)
      def defer[A](fa: => F[A]): F[A] = F.defer(fa)
      def empty[A]: F[A] = F.empty
      def combineK[A](l: F[A], r: F[A]) = F.combineK(l, r)
      def not[A](p: F[A]): F[Unit] = F.not(p)
      def runOption[A](p: F[A], str: String): Option[(String, A)] = F.runOption(p, str)
      val anyChar: F[Char] = F.anyChar
    }

  def repeatedCharConsistency(p: F[Char]): IsEq[F[String]] =
    // repeated on parsers that accept empty is not safe
    //
    if (F.runOption(p, "").isDefined) F.pure("") <-> F.pure("")
    else F.repeatedChar(p) <-> F.repeated(p).map(_.mkString)

  def parserOrNot(str: String, p: F[Unit]): Boolean = {
    val works = F.runOption(p, str).isDefined
    val notWorks = F.runOption(F.not(p), str).isDefined

    // this is XOR
    (works && (!notWorks)) || (!works && notWorks)
  }

  def charOfHeadParses(s: String): Boolean =
    s.isEmpty || {
      val c = s.head
      F.runOption(F.char(c), s) == Some((s.tail, ()))
    }

  def anyCharParser(s: String): Boolean =
    if (s.isEmpty) F.runOption(F.anyChar, s) == None
    else F.runOption(F.anyChar, s) == Some((s.tail, s.head))

  def oneOfMatchesOneOfChar(ps: Set[Char]): IsEq[F[Char]] =
    F.oneOfChar(ps) <-> F.oneOf(ps.toList.map { c => F.as(F.char(c), c) })

  // The following are reference checks
  def stringMatchesReference(s: String): IsEq[F[Unit]] =
    F.string(s) <-> referenceF.string(s)

  def oneOfCharMatchesReference(cs: Set[Char]): IsEq[F[Char]] =
    F.oneOfChar(cs) <-> referenceF.oneOfChar(cs)

}

object ParserALaws {
  def apply[F[_]](implicit ev: ParserA[F]): ParserALaws[F] =
    new ParserALaws[F] { def F: ParserA[F] = ev }
}
