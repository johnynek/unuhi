package org.bykn.unuhi

import cats.{Alternative, Defer, Eval, Eq}
import cats.data.NonEmptyList

trait ParserA[P[_]] extends Alternative[P] with Defer[P] {
  def char(c: Char): P[Unit]
  // Consumes 0 bytes on success, which happens only
  // when the given parser would NOT succeed at the current
  // location
  def not[A](p: P[A]): P[Unit]
  def runOption[A](p: P[A], str: String): Option[(String, A)]
  def anyChar: P[Char]

  // All the rest have default implementations, but for
  // performance you will often want to override them
  def string(str: String): P[Unit] =
    if (str == "") unit
    else if (str.length == 1) char(str.charAt(0))
    else void(product(char(str.charAt(0)), defer(string(str.tail))))

  def oneOfChar(chars: Set[Char]): P[Char] =
    oneOf(chars.toList.map { c => productR(char(c))(pure(c)) })

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

  def optional[A](p: P[A]): P[Option[A]] =
    combineK(map(p)(Some(_)), pure(None))

  def repeated[A](p: P[A]): P[List[A]] = {
    val empty = pure(List.empty[A])
    def atLeastOne: P[List[A]] = map2(p, defer(result)) (_ :: _)
    lazy val result: P[List[A]] = combineK(atLeastOne, empty)

    result
  }

  def repeatedChar(p: P[Char]): P[String] =
    map(repeated(p))(_.mkString)

  def repeated1[A](p: P[A]): P[NonEmptyList[A]] =
    map2(p, repeated(p))(NonEmptyList(_, _))

  def repeated1Char(p: P[Char]): P[String] =
    map2(p, repeatedChar(p)) { (c, str) => s"$c$str" }

  def repeated_[A](p: P[A]): P[Unit] = {
    def atLeastOne: P[Unit] = void(product(p, defer(result)))
    lazy val result: P[Unit] = combineK(atLeastOne, unit)

    result
  }

  def repeated1_[A](p: P[A]): P[Unit] =
    void(product(p, repeated_(p)))

  /**
   * Since we can defer creating of a parser, we can generally implement this faster
   */
  override def map2Eval[A, B, Z](fa: P[A], fb: Eval[P[B]])(f: (A, B) => Z): Eval[P[Z]] =
    Eval.later(map2(fa, defer(fb.value))(f))

  override def replicateA[A](cnt: Int, fa: P[A]): P[List[A]] =
    if (cnt <= 0) pure(Nil)
    else map2(fa, defer(replicateA(cnt - 1, fa)))(_ :: _)

  def replicateAChar(cnt: Int, fa: P[Char]): P[String] =
    map(replicateA(cnt, fa))(_.mkString)

  // Just 1 space or tab
  val whiteSpace1: P[Unit] =
    void(oneOfChar(Set(' ', '\t')))

  // 0 or more whitespace1
  val whiteSpace: P[Unit] =
    repeated_(whiteSpace1)

  val newline1: P[Unit] =
    char('\n')

  val digit: P[Char] =
    oneOfChars("0123456789")

  val nonZeroDigit: P[Char] =
    oneOfChars("123456789")
}

object ParserA {
  def apply[P[_]](implicit p: ParserA[P]): ParserA[P] = p

  def runsSame[P[_]: ParserA, A](tests: List[String]): Eq[P[A]] =
    new Eq[P[A]] {
      def eqv(a: P[A], b: P[A]): Boolean = {
        val pa = ParserA[P]
        tests.forall { str =>
          pa.runOption(a, str) == pa.runOption(b, str)
        }
      }
    }
}
