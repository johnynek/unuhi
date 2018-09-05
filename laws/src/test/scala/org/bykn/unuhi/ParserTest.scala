package org.bykn.unuhi

import org.bykn.unuhi.laws.ParserASuite

class ParserSuite extends ParserASuite[Parser] {
  def name = "Parser"
  def parserA = Parser.parserA
}
