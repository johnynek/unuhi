package org.bykn.unuhi.laws

import org.bykn.unuhi._
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Gen}
import cats.Eq

import cats.implicits._

abstract class ParserASuite[P[_]] extends CatsSuite {
  def name: String
  implicit def parserA: ParserA[P]

  implicit lazy val arbPInt: Arbitrary[P[Int]] = Arbitrary {
    val recurse = Gen.lzy(arbPInt.arbitrary)
    val const: Gen[P[Int]] = Gen.choose(Int.MinValue, Int.MaxValue).map(parserA.pure(_))

    // can we parse a simple int
    val posInt: P[Int] = {
      val d = parserA.digit
      def digitCnt(n: Int): P[String] =
        if (n <= 0) parserA.empty
        else if (n == 1) parserA.digit.map(_.toString)
        else (parserA.digit, parserA.defer(digitCnt(n - 1))).mapN(_ + _)

      digitCnt(8).map(_.toInt)
    }

    // count of consecutive things:
    def len[A](p: P[A]): P[Int] =
      parserA.repeated(p).map(_.size)

    val lengthOfAny: Gen[P[Int]] = Gen.const(len(parserA.anyChar))
    val consecutiveAscii: Gen[P[Int]] =
      Gen.const(len(parserA.oneOfChar(('A' to 'z').toSet)))

    val oneOfP = Gen.listOf(recurse).map(parserA.oneOf(_))

    Gen.frequency(
      (4, const),
      (3, lengthOfAny),
      (1, consecutiveAscii),
      (1, Gen.const(parserA.empty)),
      (1, Gen.lzy(Gen.zip(arbFnInt.arbitrary, recurse)).map { case (fn, p) => parserA.ap(fn)(p) }),
      (1, Gen.const(posInt)),
      (1, oneOfP)
      )
  }

  implicit val arbCharP: Arbitrary[P[Char]] =
    Arbitrary(arbPInt.arbitrary.map { pi => pi.map { i => (i & 0xfff).toChar } })

  implicit lazy val arbFnInt: Arbitrary[P[Int => Int]] = Arbitrary {
    val recurse = Gen.lzy(arbFnInt.arbitrary)
    val const: Gen[P[Int => Int]] = Arbitrary.arbitrary[Int => Int].map(parserA.pure(_))

    val sum: Gen[P[Int => Int]] =
      arbPInt.arbitrary.map { pi: P[Int] => pi.map { i => { x: Int => i + x } } }

    val compose: Gen[P[Int => Int]] =
      Gen.zip(recurse, recurse).map { case (fn1, fn2) =>
        parserA.map2(fn1, fn2)(_.andThen(_))
      }

    Gen.frequency(
      (3, const),
      (2, Gen.const(parserA.empty)),
      (1, sum),
      (1, compose)
    )
  }

  implicit val arbFUnit: Arbitrary[P[Unit]] =
    Arbitrary(Gen.oneOf(arbCharP.arbitrary.map(_.void), arbFnInt.arbitrary.map(_.void)))


  checkAll(name, ParserATests[P].parserA[Int, Int, Int])
}
