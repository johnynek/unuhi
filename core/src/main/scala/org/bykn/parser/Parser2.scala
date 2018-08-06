package org.bykn.parser

sealed trait Parser2[+A] {
  import Parser2._

  final def parse(str: String): Either[Error, (String, A)] = {
    val state = new Parser2.State(str)
    val res = parseMutable(state)
    if (!state.failed) Right((str.substring(state.offset), res))
    else Left(state.error)
  }

  protected def parseMutable(state: Parser2.State): A
}

object Parser2 {

  private class State(val inputStr: String) {
    var offset: Int = 0
    var error: Error = null

    def failed: Boolean = (error != null)

    def resetTo(off: Int): Unit = {
      offset = off
      error = null
    }

    def increment(cnt: Int): Unit = {
      offset += cnt
    }

    def currentStr: String = inputStr.substring(offset)

    def fail(err: Error): Unit = {
      error = err
    }
  }

  implicit val parserA: ParserA[Parser2] =
    new ParserA[Parser2] {
      def anyChar = AnyChar
      def defer[A](p: => Parser2[A]): Parser2[A] = DeferP(p _)
      def pure[A](a: A) = Const(a)
      override def product[A, B](a: Parser2[A], b: Parser2[B]) = Zip(a, b)
      override def oneOf[A](ps: List[Parser2[A]]) = OneOf(ps)
      def combineK[A](p1: Parser2[A], p2: Parser2[A]) = OneOf(p1 :: p2 :: Nil)
      def string(str: String) = StringP(str)
      def runOption[A](p: Parser2[A], str: String) =
        p.parse(str).right.toOption
      def empty[A] = Parser2.failed

      def not[A](p: Parser2[A]) = NotP(p)
      def oneOfChar(cs: Set[Char]) = Chars(cs)

      override def map[A, B](p: Parser2[A])(fn: A => B): Parser2[B] =
        MapP(p, fn)

      def ap[A, B](p: Parser2[A => B])(pa: Parser2[A]): Parser2[B] =
        map(product(p, pa)) { case (fn, a) => fn(a) }

      override def widen[A, B >: A](p: Parser2[A]): Parser2[B] = p
    }

  val failed: Parser2[Nothing] = OneOf(Nil)

  sealed trait Error {
    def rest: String
  }
  object Error {
    case class Unexpected(rest: String, expected: String) extends Error
    case class Exhausted[A](rest: String, parsers: List[Parser2[A]]) extends Error
    case class UnexpectedChar(rest: String, allowed: Set[Char]) extends Error
    case class ExpectedFailure[A](rest: String, ofParser: Parser2[A]) extends Error
  }

  private case object AnyChar extends Parser2[Char] {
    def parseMutable(state: Parser2.State): Char =
      if (state.inputStr.length > state.offset) {
        val res = state.inputStr.charAt(state.offset)
        state.increment(1)
        res
      }
      else {
        state.fail(Error.Unexpected(state.currentStr, ""))
        0.toChar
      }
  }
  private case class StringP(expect: String) extends Parser2[Unit] {
    def parseMutable(state: Parser2.State): Unit = {
      if (state.inputStr.startsWith(expect, state.offset)) {
        state.increment(expect.length)
      }
      else {
        state.fail(Error.Unexpected(state.currentStr, expect))
      }
      ()
    }
  }

  private case class Const[A](a: A) extends Parser2[A] {
    def parseMutable(state: Parser2.State) = a
  }

  private case class NotP[A](p: Parser2[A]) extends Parser2[Unit] {
    def parseMutable(state: Parser2.State) = {
      val offset = state.offset
      p.parseMutable(state)
      if (state.failed) {
        state.resetTo(offset)
        ()
      }
      else {
        state.resetTo(offset)
        state.fail(Error.ExpectedFailure(state.currentStr, p))
        ()
      }
    }
  }

  private case class Chars(pred: Set[Char]) extends Parser2[Char] {
    def parseMutable(state: Parser2.State) =
      if (state.inputStr.length > state.offset) {
        val c = state.inputStr.charAt(state.offset)
        if (pred(c)) {
          state.increment(1)
          c
        }
        else {
          state.fail(Error.UnexpectedChar(state.currentStr, pred))
          0.toChar
        }
      }
      else {
        state.fail(Error.UnexpectedChar("", pred))
        0.toChar
      }
  }

  private case class Zip[A, B](a: Parser2[A], b: Parser2[B]) extends Parser2[(A, B)] {
    def parseMutable(state: Parser2.State) = {
      val resa = a.parseMutable(state)
      if (!state.failed) {
        val resb = b.parseMutable(state)
        if (!state.failed) (resa, resb)
        else null // don't bother to allocate in the failed case
      }
      else {
        null
      }
    }
  }

  private case class OneOf[A](as: List[Parser2[A]]) extends Parser2[A] {
    def parseMutable(state: Parser2.State) = {
      @annotation.tailrec
      def loop(ps: List[Parser2[A]]): A =
        ps match {
          case Nil =>
            state.fail(Error.Exhausted(state.currentStr, ps))
            null.asInstanceOf[A]
          case h :: t =>
            val offset = state.offset
            val res = h.parseMutable(state)
            if (state.failed) {
              state.resetTo(offset)
              loop(t)
            }
            else {
              res
            }
        }
      loop(as)
    }
  }

  private case class DeferP[A](makeP: () => Parser2[A]) extends Parser2[A] {
    lazy val p = makeP()
    def parseMutable(state: Parser2.State) = p.parseMutable(state)
  }

  private case class MapP[A, B](p: Parser2[A], fn: A => B) extends Parser2[B] {
    def parseMutable(state: Parser2.State) = {
      val res = p.parseMutable(state)
      if (state.failed) null.asInstanceOf[B]
      else fn(res)
    }
  }
}
