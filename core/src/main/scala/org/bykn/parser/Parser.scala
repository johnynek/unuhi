package org.bykn.parser

sealed trait Parser[+A] {
  import Parser._

  def parse(str: String): Either[Error, (String, A)]

  def zip[B](that: Parser[B]): Parser[(A, B)] =
    Zip(this, that)

  def orElse[A1 >: A](that: Parser[A1]): Parser[A1] =
    OneOf(that :: that :: Nil)

  def unary_! : Parser[Unit] =
    NotP(this)

  def map[B](fn: A => B): Parser[B] =
    MapP(this, fn)

  def flatMap[B](fn: A => Parser[B]): Parser[B] =
    TailRecM[Option[A], B](Option.empty,
      {
        case None => this.map { a => Left(Some(a)): Either[Option[A], B] }
        case Some(a) => fn(a).map { b => Right(b): Either[Option[A], B] }
      })
}

object Parser {
  implicit val parserM: ParserM[Parser] =
    new ParserM[Parser] {
      def anyChar = AnyChar
      def defer[A](p: => Parser[A]): Parser[A] = DeferP(p _)
      def pure[A](a: A) = Const(a)
      override def product[A, B](a: Parser[A], b: Parser[B]) = Zip(a, b)
      override def oneOf[A](ps: List[Parser[A]]) = OneOf(ps)
      def combineK[A](p1: Parser[A], p2: Parser[A]) = OneOf(p1 :: p2 :: Nil)
      def string(str: String) = StringP(str)
      def runOption[A](p: Parser[A], str: String) =
        p.parse(str).right.toOption
      def empty[A] = Parser.failed

      def not[A](p: Parser[A]) = NotP(p)
      def oneOfChar(cs: Set[Char]) = Chars(cs)

      def tailRecM[A, B](a: A)(fn: A => Parser[Either[A, B]]) =
        TailRecM(a, fn)

      override def map[A, B](p: Parser[A])(fn: A => B): Parser[B] =
        MapP(p, fn)

      def flatMap[A, B](p: Parser[A])(fn: A => Parser[B]): Parser[B] =
        p.flatMap(fn)

      override def widen[A, B >: A](p: Parser[A]): Parser[B] = p
    }

  def oneOf[A](ps: List[Parser[A]]): Parser[A] =
    OneOf(ps)

  def string(str: String): Parser[Unit] =
    StringP(str)

  def oneOfChar(cset: Set[Char]): Parser[Char] =
    Chars(cset)

  val failed: Parser[Nothing] =
    OneOf(Nil)

  def const[A](a: A): Parser[A] =
    Const(a)

  def anyChar: Parser[Char] = AnyChar

  sealed trait Error {
    def rest: String
  }
  object Error {
    case class Unexpected(rest: String, expected: String) extends Error
    case class Exhausted[A](rest: String, parsers: List[Parser[A]]) extends Error
    case class UnexpectedChar(rest: String, allowed: Set[Char]) extends Error
    case class ExpectedFailure[A](rest: String, ofParser: Parser[A]) extends Error
  }

  private case object AnyChar extends Parser[Char] {
    def parse(s: String) =
      if (s.length > 0) Right((s.substring(1), s.charAt(0)))
      else Left(Error.Unexpected(s, ""))
  }
  private case class StringP(expect: String) extends Parser[Unit] {
    def parse(str: String) =
      if (str.startsWith(expect)) Right((str.substring(expect.length), ()))
      else Left(Error.Unexpected(str, expect))
  }

  private case class Const[A](a: A) extends Parser[A] {
    def parse(str: String) = Right((str, a))
  }

  private case class NotP[A](p: Parser[A]) extends Parser[Unit] {
    def parse(str: String) =
      p.parse(str) match {
        case Right(_) => Left(Error.ExpectedFailure(str, p))
        case Left(_) => Right((str, ()))
      }
  }

  private case class Chars(pred: Set[Char]) extends Parser[Char] {
    def parse(str: String) =
      if (str.length > 0) {
        val c = str.charAt(0)
        if (pred(c)) Right((str.substring(1), c))
        else Left(Error.UnexpectedChar(str, pred))
      }
      else Left(Error.UnexpectedChar(str, pred))
  }

  private case class Zip[A, B](a: Parser[A], b: Parser[B]) extends Parser[(A, B)] {
    def parse(str: String) =
      a.parse(str) match {
        case Left(e) => Left(e)
        case Right((next, a)) =>
          b.parse(next)
            .right
            .map { case (rest, b) => (rest, (a, b)) }
      }
  }

  private case class OneOf[A](as: List[Parser[A]]) extends Parser[A] {
    def parse(str: String) = {
      @annotation.tailrec
      def loop(ps: List[Parser[A]]): Either[Error, (String, A)] =
        ps match {
          case Nil => Left(Error.Exhausted(str, ps))
          case h :: t =>
            h.parse(str) match {
              case Left(_) => loop(t)
              case right => right
            }
        }
      loop(as)
    }
  }

  private case class DeferP[A](makeP: () => Parser[A]) extends Parser[A] {
    lazy val p = makeP()
    def parse(str: String) = p.parse(str)
  }

  private case class MapP[A, B](p: Parser[A], fn: A => B) extends Parser[B] {
    def parse(str: String) = p.parse(str).right.map { case (s, a) => (s, fn(a)) }
  }

  private case class TailRecM[A, B](a: A, fn: A => Parser[Either[A, B]]) extends Parser[B] {
    def parse(str: String) = {
      def loop(a: A, str: String): Either[Error, (String, B)] =
        fn(a).parse(str) match {
          case Left(e) => Left(e)
          case Right((str, Left(a))) => loop(a, str)
          case Right((str, Right(b))) => Right((str, b))
        }

      loop(a, str)
    }
  }
}
