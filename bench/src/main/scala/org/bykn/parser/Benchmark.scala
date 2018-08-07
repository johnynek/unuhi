package org.bykn.parser.benchmark

import org.bykn.parser._

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.util.Random

@State(Scope.Benchmark)
class JsonBenchmark {
  @Param(Array("5", "25"))
  var length: Int = 0
  @Param(Array("1"))
  var depth: Int = 0

  var jsons: Array[String] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    val rng = new Random("parser".hashCode)

    def randomAsciiChar(): Char =
      (rng.nextInt(126 - 32) + 32).toChar

    def randomAscii(): String = {
      val length = rng.nextInt(100)
      (0 to length).map { _ => randomAsciiChar() }.mkString
    }

    def makeDepth(d: Int): Json =
      if (d <= 0) {
        if (rng.nextBoolean) Json.JString(randomAscii())
        else Json.Number(BigInt(rng.nextInt(Int.MaxValue) - 1000))
      }
      else {
        // Make an array of the right length:
        Json.JList((0 until length).iterator.map(_ => makeDepth(d - 1)).toList)
      }

    jsons = (0 until 100).iterator.map(_ => makeDepth(depth).repr).toArray
  }

  // @Benchmark
  // def timeParsing(bh: Blackhole): Unit = {
  //   val parser = Json.parser[Parser]
  //   var idx = 0
  //   while(idx < jsons.length) {
  //     val jsonStr = jsons(idx)
  //     val res = parser.parse(jsonStr)
  //     bh.consume(res)
  //     require(res.isRight)
  //     idx = idx + 1
  //   }
  // }

  @Benchmark
  def timeParsing2(bh: Blackhole): Unit = {
    val parser = Json.parser[Parser2]
    var idx = 0
    while(idx < jsons.length) {
      val jsonStr = jsons(idx)
      val res = parser.parse(jsonStr)
      bh.consume(res)
      require(res.isRight)
      idx = idx + 1
    }
  }

  @Benchmark
  def timeParsing3(bh: Blackhole): Unit = {
    val parser = Json.parser(FastparseParser.fastParseParserA)
    var idx = 0
    while(idx < jsons.length) {
      val jsonStr = jsons(idx)
      val res = parser.parse(jsonStr)
      bh.consume(res)
      idx = idx + 1
    }
  }
}
