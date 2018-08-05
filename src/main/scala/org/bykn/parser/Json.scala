package org.bykn.parser

import cats.implicits._

sealed trait Json {
  def repr: String
}

object Json {
  case class Number(toLong: BigInt) extends Json {
    def repr = toLong.toString
  }
  case class JList(items: List[Json]) extends Json {
    def repr = items.iterator.map(_.repr).mkString("[", ", ", "]")
  }
  case class JMap(toMap: Map[String, Json]) extends Json {
    def repr = "{}"
  }
  case class JString(toStr: String) extends Json {
    def repr = toStr.iterator.flatMap {
      case '"' => "\\\""
      case '\\' => "\\\\"
      case c => c.toString
    }.mkString("\"", "", "\"")
  }

  def parser[P[_]](implicit p: ParserT[P]): P[Json] = {

    val recurse: P[Json] = p.defer(parser)

    val numP: P[Number] = {
      val pos = (p.nonZeroDigit, p.repeated(p.digit)).mapN { (c0, cs) =>
        Number(BigInt((c0 :: cs).mkString))
      }
      val zero = p.char('0') *> p.pure(Number(BigInt(0)))
      val neg = (p.char('-') *> pos).map { case Number(b) => Number(-b) }

      p.oneOf(List(pos, neg, zero))
    }

    val stringP: P[JString] = {
      val escaped: Map[Char, Char] = Map('t' -> '\t', '\\' -> '\\', '"' -> '"', 'n' -> '\n')
      val q = p.char('"')
      val slash = p.char('\\')
      val notQuote = p.not(p.combineK(q, slash)) *> p.anyChar
      val escapedC = (slash *> p.oneOfChar(escaped.keySet)).map(escaped)
      val charP = notQuote combineK escapedC
      (q *> p.repeated(charP) <* q).map { cs => JString(cs.mkString) }
    }

    val ws = p.repeated_(p.oneOfChar(Set(' ', '\t', '\n')))

    val listP: P[JList] = {
      val oneAndSpace = recurse <* ws
      val sep = p.char(',') *> ws

      val jlist1 =
        (oneAndSpace, p.repeated(sep *> oneAndSpace))
          .mapN { (h, t) =>
            JList(h :: t)
          }

      val list0 = p.combineK(jlist1, p.pure(JList(Nil)))
      p.char('[') *> list0 <* p.char(']')
    }

    p.oneOf(List(numP.widen, listP.widen, stringP.widen))
  }
}
