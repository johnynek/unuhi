package org.bykn.unuhi

import org.bykn.unuhi.laws.ParserASuite
import org.scalacheck.{Arbitrary, Gen}
import cats.Eq

class ParserSuite extends ParserASuite[Parser] {
  def name = "Parser"
  def parserA = Parser.parserA
}
