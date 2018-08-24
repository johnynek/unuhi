package org.bykn.unuhi

import cats.data.NonEmptyList

sealed trait Parser[+A] {
  import Parser._

  final def parse(str: String): Either[Error, (String, A)] = {
    val state = new Parser.State(str)
    val res = parseMutable(state)
    if (!state.failed) Right((str.substring(state.offset), res))
    else Left(Error.Message(state.offset, str, state.error))
  }

  protected def parseMutable(state: Parser.State): A
}

object Parser {

  private class State(val inputStr: String) {
    var offset: Int = 0
    var error: String = null

    def failed: Boolean = (error != null)

    def resetTo(off: Int): Unit = {
      offset = off
      error = null
    }

    def increment(cnt: Int): Unit = {
      offset += cnt
    }

    def fail(off: Int, msg: String): Unit = {
      error = msg
      offset = off
    }
  }

  implicit val parserA: ParserA[Parser] =
    new ParserA[Parser] {
      def anyChar = AnyChar
      def defer[A](p: => Parser[A]): Parser[A] = DeferP(p _)
      def pure[A](a: A) = Const(a)
      override val unit = Const(())
      override def as[A, B](pa: Parser[A], b: B): Parser[B] = ZipR(pa, Const(b))
      override def product[A, B](a: Parser[A], b: Parser[B]) = Zip(a, b)
      override def productL[A, B](a: Parser[A])(b: Parser[B]) = ZipL(a, b)
      override def productR[A, B](a: Parser[A])(b: Parser[B]) = ZipR(a, b)
      override def oneOf[A](ps: List[Parser[A]]) = OneOf(ps.toArray)
      def combineK[A](p1: Parser[A], p2: Parser[A]) = CombineK(p1, p2)
      override def string(str: String) = StringP(str)
      override def char(c: Char) = CharP(c)
      def runOption[A](p: Parser[A], str: String) =
        p.parse(str).right.toOption
      def empty[A] = Parser.failed

      def not[A](p: Parser[A]) = NotP(p)
      override def oneOfChar(cs: Set[Char]) = Chars(cs.toArray)

      override def map[A, B](p: Parser[A])(fn: A => B): Parser[B] =
        MapP(p, fn)

      def ap[A, B](p: Parser[A => B])(pa: Parser[A]): Parser[B] =
        map(product(p, pa)) { case (fn, a) => fn(a) }

      override def widen[A, B >: A](p: Parser[A]): Parser[B] = p
      override def repeated[A](p: Parser[A]): Parser[List[A]] =
        Repeated(0, p, true)

      override def repeated1[A](p: Parser[A]): Parser[NonEmptyList[A]] =
        MapP[List[A], NonEmptyList[A]](Repeated(1, p, true), NonEmptyList.fromListUnsafe(_))

      override def repeated_[A](p: Parser[A]): Parser[Unit] =
        void(Repeated(0, p, false))

      override def repeated1_[A](p: Parser[A]): Parser[Unit] =
        void(Repeated(1, p, false))

      override def void[A](p: Parser[A]): Parser[Unit] =
        p match {
          case void@VoidP(_) => void
          case notVoid => VoidP(notVoid)
        }

    }

  val failed: Parser[Nothing] = OneOf[Nothing](new Array(0))

  sealed trait Error {
    def rest: String = original.substring(offset)
    def offset: Int
    def original: String
    def message: String
  }
  object Error {
    case class Message(offset: Int, original: String, message: String) extends Error
  }

  private case object AnyChar extends Parser[Char] {
    def parseMutable(state: Parser.State): Char =
      if (state.inputStr.length > state.offset) {
        val res = state.inputStr.charAt(state.offset)
        state.increment(1)
        res
      }
      else {
        state.fail(state.offset, "reached EOF, expected any character")
        '0'
      }
  }
  private case class CharP(expect: Char) extends Parser[Unit] {
    val msg = s"expected $expect"
    def parseMutable(state: Parser.State): Unit = {
      if (state.inputStr.length > state.offset && state.inputStr.charAt(state.offset) == expect) {
        state.increment(1)
      }
      else {
        state.fail(state.offset, msg)
      }
      ()
    }
  }
  private case class StringP(expect: String) extends Parser[Unit] {
    val msg = s"expected $expect"
    def parseMutable(state: Parser.State): Unit = {
      if (state.inputStr.startsWith(expect, state.offset)) {
        state.increment(expect.length)
      }
      else {
        state.fail(state.offset, msg)
      }
      ()
    }
  }

  private case class Const[A](a: A) extends Parser[A] {
    def parseMutable(state: Parser.State) = a
  }

  private case class NotP[A](p: Parser[A]) extends Parser[Unit] {
    val msg = s"not parser failed, because underlying succeeded: $p"
    def parseMutable(state: Parser.State) = {
      val offset = state.offset
      p.parseMutable(state)
      if (state.failed) {
        state.resetTo(offset)
        ()
      }
      else {
        state.resetTo(offset)
        state.fail(offset, msg)
        ()
      }
    }
  }

  private case class Chars(pred: Array[Char]) extends Parser[Char] {
    import java.util.Arrays
    Arrays.sort(pred)

    val msg = {
      val predStr = pred.mkString(", ")
      s"character not in $predStr"
    }

    def parseMutable(state: Parser.State) =
      if (state.inputStr.length > state.offset) {
        val c = state.inputStr.charAt(state.offset)
        if (Arrays.binarySearch(pred, c) >= 0) {
          state.increment(1)
          c
        }
        else {
          state.fail(state.offset, msg)
          '0'
        }
      }
      else {
        state.fail(state.offset, msg)
        '0'
      }
  }

  private case class Zip[A, B](a: Parser[A], b: Parser[B]) extends Parser[(A, B)] {
    def parseMutable(state: Parser.State) = {
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

  private case class ZipR[A, B](a: Parser[A], b: Parser[B]) extends Parser[B] {
    def parseMutable(state: Parser.State) = {
      val resa = a.parseMutable(state)
      if (!state.failed) {
        b.parseMutable(state)
      }
      else {
        null.asInstanceOf[B]
      }
    }
  }

  private case class ZipL[A, B](a: Parser[A], b: Parser[B]) extends Parser[A] {
    def parseMutable(state: Parser.State) = {
      val resa = a.parseMutable(state)
      if (!state.failed) {
        b.parseMutable(state)
      }
      resa
    }
  }

  private case class OneOf[A](as: Array[Parser[A]]) extends Parser[A] {
    def parseMutable(state: Parser.State) = {
      var idx = 0
      var res: A = null.asInstanceOf[A]
      val offset = state.offset
      while (idx < as.length) {
        res = as(idx).parseMutable(state)
        if (state.failed) {
          state.resetTo(offset)
          res = null.asInstanceOf[A]
          idx += 1
        }
        else {
          idx = Int.MaxValue
        }
      }
      if (res == null) state.fail(offset, "no matching parser")
      res
    }
  }

  private case class CombineK[A](first: Parser[A], second: Parser[A]) extends Parser[A] {
    def parseMutable(state: Parser.State) = {
      val offset = state.offset
      val res = first.parseMutable(state)
      if (state.failed) {
        state.resetTo(offset)
        second.parseMutable(state)
      }
      else {
        res
      }
    }
  }

  private case class DeferP[A](makeP: () => Parser[A]) extends Parser[A] {
    lazy val p = {
      @annotation.tailrec
      def loop(fn: () => Parser[A]): Parser[A] =
        fn() match {
          case DeferP(fn) => loop(fn)
          case notDefer => notDefer
        }
      loop(makeP)
    }

    def parseMutable(state: Parser.State) = p.parseMutable(state)
  }

  private case class MapP[A, B](p: Parser[A], fn: A => B) extends Parser[B] {
    def parseMutable(state: Parser.State) = {
      val res = p.parseMutable(state)
      if (state.failed) null.asInstanceOf[B]
      else fn(res)
    }
  }

  private case class VoidP[A](p: Parser[A]) extends Parser[Unit] {
    def parseMutable(state: Parser.State) = {
      p.parseMutable(state)
      ()
    }
  }

  private case class Repeated[A](cnt: Int, p: Parser[A], keepRes: Boolean) extends Parser[List[A]] {
    val msg = "failed to repeatedly parse at least $cnt items"
    def parseMutable(state: Parser.State) = {
      var remaining = cnt
      val listbldr = if (keepRes) List.newBuilder[A] else null
      var offset: Int = 0
      while (!state.failed) {
        offset = state.offset
        val res = p.parseMutable(state)
        if (!state.failed) {
          remaining -= 1
          if (keepRes) listbldr += res
        }
      }
      // if we get here we must have finally failed
      if (remaining <= 0) {
        // don't need any more
        state.resetTo(offset)
        if (keepRes) listbldr.result() else Nil
      }
      else {
        // we actually did fail
        state.fail(offset, msg)
        Nil
      }
    }
  }
}
