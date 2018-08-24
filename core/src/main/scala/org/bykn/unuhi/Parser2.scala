package org.bykn.parser

import cats.data.NonEmptyList

sealed trait Parser2[+A] {
  import Parser2._

  final def parse(str: String): Either[Error, (String, A)] = {
    val state = new Parser2.State(str)
    val res = parseMutable(state)
    if (!state.failed) Right((str.substring(state.offset), res))
    else Left(Error.Message(state.offset, str, state.error))
  }

  protected def parseMutable(state: Parser2.State): A
}

object Parser2 {

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

  implicit val parserA: ParserA[Parser2] =
    new ParserA[Parser2] {
      def anyChar = AnyChar
      def defer[A](p: => Parser2[A]): Parser2[A] = DeferP(p _)
      def pure[A](a: A) = Const(a)
      override def product[A, B](a: Parser2[A], b: Parser2[B]) = Zip(a, b)
      override def productL[A, B](a: Parser2[A])(b: Parser2[B]) = ZipL(a, b)
      override def productR[A, B](a: Parser2[A])(b: Parser2[B]) = ZipR(a, b)
      override def oneOf[A](ps: List[Parser2[A]]) = OneOf(ps.toArray)
      def combineK[A](p1: Parser2[A], p2: Parser2[A]) = CombineK(p1, p2)
      def string(str: String) = StringP(str)
      override def char(c: Char) = CharP(c)
      def runOption[A](p: Parser2[A], str: String) =
        p.parse(str).right.toOption
      def empty[A] = Parser2.failed

      def not[A](p: Parser2[A]) = NotP(p)
      def oneOfChar(cs: Set[Char]) = Chars(cs.toArray)

      override def map[A, B](p: Parser2[A])(fn: A => B): Parser2[B] =
        MapP(p, fn)

      def ap[A, B](p: Parser2[A => B])(pa: Parser2[A]): Parser2[B] =
        map(product(p, pa)) { case (fn, a) => fn(a) }

      override def widen[A, B >: A](p: Parser2[A]): Parser2[B] = p
      override def repeated[A](p: Parser2[A]): Parser2[List[A]] =
        Repeated(0, p, true)

      override def repeated1[A](p: Parser2[A]): Parser2[NonEmptyList[A]] =
        MapP[List[A], NonEmptyList[A]](Repeated(1, p, true), NonEmptyList.fromListUnsafe(_))

      override def repeated_[A](p: Parser2[A]): Parser2[Unit] =
        void(Repeated(0, p, false))

      override def repeated1_[A](p: Parser2[A]): Parser2[Unit] =
        void(Repeated(1, p, false))

    }

  val failed: Parser2[Nothing] = OneOf[Nothing](new Array(0))

  sealed trait Error {
    def rest: String = original.substring(offset)
    def offset: Int
    def original: String
    def message: String
  }
  object Error {
    case class Message(offset: Int, original: String, message: String) extends Error
  }

  private case object AnyChar extends Parser2[Char] {
    def parseMutable(state: Parser2.State): Char =
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
  private case class CharP(expect: Char) extends Parser2[Unit] {
    val msg = s"expected $expect"
    def parseMutable(state: Parser2.State): Unit = {
      if (state.inputStr.length > state.offset && state.inputStr.charAt(state.offset) == expect) {
        state.increment(1)
      }
      else {
        state.fail(state.offset, msg)
      }
      ()
    }
  }
  private case class StringP(expect: String) extends Parser2[Unit] {
    val msg = s"expected $expect"
    def parseMutable(state: Parser2.State): Unit = {
      if (state.inputStr.startsWith(expect, state.offset)) {
        state.increment(expect.length)
      }
      else {
        state.fail(state.offset, msg)
      }
      ()
    }
  }

  private case class Const[A](a: A) extends Parser2[A] {
    def parseMutable(state: Parser2.State) = a
  }

  private case class NotP[A](p: Parser2[A]) extends Parser2[Unit] {
    val msg = s"not parser failed, because underlying succeeded: $p"
    def parseMutable(state: Parser2.State) = {
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

  private case class Chars(pred: Array[Char]) extends Parser2[Char] {
    import java.util.Arrays
    Arrays.sort(pred)

    val msg = {
      val predStr = pred.mkString(", ")
      s"character not in $predStr"
    }

    def parseMutable(state: Parser2.State) =
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

  private case class ZipR[A, B](a: Parser2[A], b: Parser2[B]) extends Parser2[B] {
    def parseMutable(state: Parser2.State) = {
      val resa = a.parseMutable(state)
      if (!state.failed) {
        b.parseMutable(state)
      }
      else {
        null.asInstanceOf[B]
      }
    }
  }

  private case class ZipL[A, B](a: Parser2[A], b: Parser2[B]) extends Parser2[A] {
    def parseMutable(state: Parser2.State) = {
      val resa = a.parseMutable(state)
      if (!state.failed) {
        b.parseMutable(state)
      }
      resa
    }
  }

  private case class OneOf[A](as: Array[Parser2[A]]) extends Parser2[A] {
    def parseMutable(state: Parser2.State) = {
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

  private case class CombineK[A](first: Parser2[A], second: Parser2[A]) extends Parser2[A] {
    def parseMutable(state: Parser2.State) = {
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

  private case class Repeated[A](cnt: Int, p: Parser2[A], keepRes: Boolean) extends Parser2[List[A]] {
    val msg = "failed to repeatedly parse at least $cnt items"
    def parseMutable(state: Parser2.State) = {
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
