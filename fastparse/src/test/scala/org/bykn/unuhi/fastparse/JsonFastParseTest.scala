package org.bykn.unuhi.fastparse

import org.bykn.unuhi._

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}

object JsonFastParseTest extends Properties("JsonFastParseTest") {
  val ascii: Gen[String] =
    Gen.listOf(Gen.choose(32, 126).map(_.toChar)).map(_.mkString)

  val jsonAtoms: Gen[Json] =
    Gen.oneOf(
      //Arbitrary.arbitrary[String].map(Json.JString(_)),
      ascii.map(Json.JString(_)),
      Arbitrary.arbitrary[BigInt].map(Json.Number(_)))

  def jsonGen(depth: Int): Gen[Json] = {
    if (depth < 1) jsonAtoms
    else {
      val recurse = Gen.lzy(jsonGen(depth - 1))
      Gen.oneOf(jsonAtoms, Gen.listOf(recurse).map(Json.JList(_)))
    }
  }


  val fpParser = Json.parser(FastparseParser.fastParseParserA)
  property("fastparse: parse round trips") = Prop.forAll(jsonGen(2)) { j =>
    FastparseParser.fastParseParserA.runOption(fpParser, j.repr) == Some(("", j))
  }
}
