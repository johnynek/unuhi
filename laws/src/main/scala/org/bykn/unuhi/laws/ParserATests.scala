package org.bykn.unuhi.laws

import org.bykn.unuhi._

import cats.Eq
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.{AlternativeTests, DeferTests, catsLawsIsEqToProp}
import org.scalacheck.{Arbitrary, Cogen, Gen, Prop}

import cats.implicits._
import Prop.forAll

trait ParserATests[F[_]] extends AlternativeTests[F] with DeferTests[F] {
  def laws: ParserALaws[F]

  implicit def eqF[A](implicit strArb: Arbitrary[String]): Eq[F[A]] = {
    implicit val parserAP: ParserA[F] = laws.F
    Gen.listOfN(1000, strArb.arbitrary).map { inputs =>
      ParserA.runsSame[F, A](inputs)
    }.sample.get
  }

  def parserA[A: Arbitrary: Cogen: Eq, B: Arbitrary: Cogen: Eq, C: Arbitrary: Cogen: Eq](
    implicit fa: Arbitrary[F[A]],
    fb: Arbitrary[F[B]],
    fc: Arbitrary[F[C]],
    funit: Arbitrary[F[Unit]],
    fab: Arbitrary[F[A => B]],
    fbc: Arbitrary[F[B => C]],
    fchar: Arbitrary[F[Char]],
    isoF: Isomorphisms[F]
  ): RuleSet =
    new RuleSet {
      val name: String = "parserA"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(alternative[A, B, C], defer[A])
      // to test most laws here, we need parsers that consume at least 1 character
      val props: Seq[(String, Prop)] = List(
        "repeatedChar repeated consistency" -> forAll(laws.repeatedCharConsistency _),
        "a parser or not(parser) can run" -> forAll(laws.parserOrNot _),
        "char parses first of string" -> forAll(laws.charOfHeadParses _),
        "anyChar parses first of string" -> forAll(laws.anyCharParser _),
        "oneOf matches oneOfChar" -> forAll(laws.oneOfMatchesOneOfChar _),
        "string matches reference" -> forAll(laws.stringMatchesReference _),
        "oneOfChar matches reference" -> forAll(laws.oneOfCharMatchesReference _)
      )
    }
}

object ParserATests {
  def apply[F[_]: ParserA]: ParserATests[F] =
    new ParserATests[F] { def laws: ParserALaws[F] = ParserALaws[F] }
}
