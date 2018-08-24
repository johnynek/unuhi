package org.bykn.unuhi

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
    def repr = toMap.iterator.map { case (k, v) =>
      val kstr = JString(k).repr
      s"$kstr: ${v.repr}"
    }.mkString("{", ", ", "}")
  }
  case class JString(toStr: String) extends Json {
    def repr = toStr.iterator.flatMap {
      case '"' => "\\\""
      case '\\' => "\\\\"
      case c => c.toString
    }.mkString("\"", "", "\"")
  }

  def parser[P[_]](implicit p: ParserA[P]): P[Json] = {

    val recurse: P[Json] = p.defer(parser)

    val numP: P[Number] = {
      def charToInt(c: Char): Int = c.toInt - '0'.toInt

      val pos = (p.nonZeroDigit, p.repeatedChar(p.digit)).mapN { (c0, cs) =>
        val i0 = charToInt(c0)
        if (cs.isEmpty) Number(BigInt(i0))
        else {
          val rightShifts = cs.length
          val left = i0 * (BigInt(10).pow(rightShifts))
          Number(left + BigInt(cs))
        }
      }
      val zero = p.char('0') *> p.pure(Number(BigInt(0)))
      val neg = (p.char('-') *> pos).map { case Number(b) => Number(-b) }

      p.oneOf(List(pos, neg, zero))
    }

    val stringP: P[String] = {
      val escaped: Map[Char, Char] = Map('t' -> '\t', '\\' -> '\\', '"' -> '"', 'n' -> '\n')
      val q = p.char('"')
      val slash = p.char('\\')
      val notQuote = p.not(p.combineK(q, slash)) *> p.anyChar
      val escapedC = (slash *> p.oneOfChar(escaped.keySet)).map(escaped)
      val charP = notQuote combineK escapedC
      (q *> p.repeatedChar(charP) <* q)
    }

    val ws = p.repeated_(p.oneOfChar(Set(' ', '\t', '\n')))

    def listSep[A](pa: P[A], sep: P[Unit]): P[List[A]] = {
      val jlist1 =
        (pa, p.repeated(sep *> pa)).mapN(_::_)

      p.combineK(jlist1, p.pure(Nil))
    }

    val listP: P[JList] = {
      val oneAndSpace = recurse <* ws
      val sep = p.char(',') *> ws
      val list = listSep(oneAndSpace, sep)
      p.char('[') *> list.map(JList(_)) <* p.char(']')
    }

    val mapP: P[JMap] = {
      val kv: P[(String, Json)] =
        (stringP <* (ws <* p.char(':') *> ws)).product(recurse)
      val kvs: P[List[(String, Json)]] = listSep(kv <* ws, p.char(',') *> ws)

      (p.char('{') *> ws *> kvs <* ws <* p.char('}'))
        .map { asList => JMap(asList.toMap) }
    }

    p.oneOf(List(numP.widen, stringP.map(JString(_)), listP.widen, mapP.widen))
  }
}
