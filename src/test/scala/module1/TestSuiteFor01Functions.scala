package module1

import module1.functions.{filterEven, filterOdd, isEven, isOdd}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper


class TestSuiteFor01Functions extends AnyFunSuite {
  test("isEven for 0") {
    val n = 0
    assert(isEven(n))
  }

  test("isEven for 100") {
    val n = 100
    assert(isEven(n))
  }

  test("isEven for -2") {
    val n = -2
    assert(isEven(n))
  }

  test("isOdd for -0") {
    val n = -0
    assert(!isOdd(n))
  }

  test("isOdd for 13") {
    val n = 13
    assert(isOdd(n))
  }

  test("filterEven for 1..9 list") {
    val n = 1 until  10
    val result = filterEven(n.toArray)
    result shouldBe List(2, 4, 6, 8)
  }

  test("filterOdd for 1..9 list") {
    val n = 1 until 10
    val result = filterOdd(n.toArray)
    result shouldBe List(1, 3, 5, 7, 9)
  }
}
