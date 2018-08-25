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
      Gen.oneOf(jsonAtoms,
        Gen.listOf(recurse).map(Json.JList(_)),
        Gen.listOf(Gen.zip(ascii, recurse)).map { kvs => Json.JMap(kvs.toMap) }
        )
    }
  }


  val jsonP: example.Parser[Json] = Json.parser[example.Parser]

  // this is correct, but slow
  // property("immutable: parse round trips") = Prop.forAll(jsonGen(2)) { j =>
  //   jsonP.parse(j.repr) == Right(("", j))
  // }

  val json2P: Parser[Json] = Json.parser[Parser]

  property("mutable: parse round trips") = Prop.forAll(jsonGen(2)) { j =>
    json2P.parse(j.repr) == Right(("", j))
  }
}
