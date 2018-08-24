package org.bykn.unuhi

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}

object JsonParserTest extends Properties("JsonParserTest") {
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


  val jsonP: Parser[Json] = Json.parser[Parser]

  property("immutable: parse round trips") = Prop.forAll(jsonGen(2)) { j =>
    jsonP.parse(j.repr) == Right(("", j))
  }

  val json2P: Parser2[Json] = Json.parser[Parser2]

  property("mutable: parse round trips") = Prop.forAll(jsonGen(2)) { j =>
    json2P.parse(j.repr) == Right(("", j))
  }

  val fpParser = Json.parser(FastparseParser.fastParseParserA)
  property("fastparse: parse round trips") = Prop.forAll(jsonGen(2)) { j =>
    FastparseParser.fastParseParserA.runOption(fpParser, j.repr) == Some(("", j))
  }
}
