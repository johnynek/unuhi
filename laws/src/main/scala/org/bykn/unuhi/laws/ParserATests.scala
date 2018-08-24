package org.bykn.unuhi.laws

import org.bykn.unuhi._

import cats.Eq
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.{AlternativeTests, DeferTests}
import org.scalacheck.{Arbitrary, Cogen, Prop}

import cats.implicits._

trait ParserATests[F[_]] extends AlternativeTests[F] with DeferTests[F] {
  def laws: ParserALaws[F]

  def parserA[A: Arbitrary: Cogen, B: Arbitrary: Cogen, C: Arbitrary: Cogen](
    implicit fa: Arbitrary[F[A]],
    fb: Arbitrary[F[B]],
    fc: Arbitrary[F[C]],
    fab: Arbitrary[F[A => B]],
    fbc: Arbitrary[F[B => C]],
    eqfa: Eq[F[A]],
    eqfb: Eq[F[B]],
    eqfc: Eq[F[C]],
    eqfabc: Eq[F[(A, B, C)]],
    eqfbool: Eq[F[Boolean]],
    isoF: Isomorphisms[F]
  ): RuleSet =
    new RuleSet {
      val name: String = "parserA"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(alternative[A, B, C], defer[A])
      val props: Seq[(String, Prop)] = Nil
        // "left distributivity" -> forAll(laws.alternativeLeftDistributivity[A, B] _),
        // "right distributivity" -> forAll(laws.alternativeRightDistributivity[A, B] _),
        // "right absorption" -> forAll(laws.alternativeRightAbsorption[A, B] _)
      // )
    }
}

object ParserATests {
  def apply[F[_]: ParserA]: ParserATests[F] =
    new ParserATests[F] { def laws: ParserALaws[F] = ParserALaws[F] }
}
