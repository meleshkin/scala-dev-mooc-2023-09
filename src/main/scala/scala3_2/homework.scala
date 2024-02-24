package scala3_2

import scala3_2.homework2.{CompletionArg, complete}


object homework1 extends App {
  extension (x: String) {
    def ++(y: String): Int = (x + y).toInt
  }
    println("1" ++ "33")


}

object homework2 {
  enum CompletionArg {
    case FromStringValue(s: String)
    case FromIntValue(i: Int)
    case FromFloatValue(f: Float)
  }

  object CompletionArg {
    given fromString: Conversion[String, CompletionArg] = FromStringValue(_)

    given fromInt: Conversion[Int, CompletionArg] = FromIntValue(_)

    given fromFloat: Conversion[Float, CompletionArg] = FromFloatValue(_)
  }

  def complete(x: CompletionArg) = x match
    case CompletionArg.FromStringValue(s) => s
    case CompletionArg.FromIntValue(i) => i.toString
    case CompletionArg.FromFloatValue(f) => f.toString
}

      @main def part2Ex(): Unit = {
        println(complete("String"))
        println(complete(1))
        println(complete(7f))
      }


object homework3 {
  opaque type Logarithm = Double

  object Logarithm {
    def apply(d: Double): Logarithm = math.log(d)
  }

  extension (x: Logarithm)
    def toDouble(): Logarithm =  math.exp(x)
    def + (y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
    def * (y: Logarithm): Logarithm = x * y


  @main def part3Ex(): Unit ={
    import Logarithm._

    val l = Logarithm(1.0)
    val l2 = Logarithm(2.0)
    val l3 = l * l2
    val l4 = l + l3

    println(l4)

  }
}