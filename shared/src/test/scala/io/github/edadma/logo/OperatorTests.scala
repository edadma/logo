package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class OperatorTests extends AnyFreeSpec with Matchers with Test:
  "arithmetic 1" in {
    eval("sum 3 4") shouldBe "7"
  }

  "arithmetic 2" in {
    an[Exception] should be thrownBy eval("sum 3 a")
  }

  "arithmetic 3" in {
    eval("sum -3 4") shouldBe "1"
  }

  "arithmetic 4" in {
    eval("difference 3 4") shouldBe "-1"
  }

  "arithmetic 5" in {
    eval("+ 3 4") shouldBe "7"
  }

  "arithmetic 6" in {
    eval("- 3 4") shouldBe "-1"
  }

  "arithmetic 7" in {
    eval("remainder 10 4") shouldBe "2"
  }

  "arithmetic 8" in {
    eval("negate 3") shouldBe "-3"
  }

  "arithmetic 9" in {
    eval("negate -3") shouldBe "3"
  }

  "comparison 1" in {
    eval("equalp 5 5") shouldBe "true"
  }

  // Infix tests
  "infix 1" in {
    eval("3 + 4") shouldBe "7"
  }

  "infix 2" in {
    eval("10 - 3") shouldBe "7"
  }

  "infix 3" in {
    eval("3 * 4") shouldBe "12"
  }

  "infix 4" in {
    eval("12 / 4") shouldBe "3"
  }

  "infix precedence 1" in {
    eval("2 + 3 * 4") shouldBe "14"
  }

  "infix precedence 2" in {
    eval("2 * 3 + 4") shouldBe "10"
  }

  "infix precedence 3" in {
    eval("10 - 2 * 3") shouldBe "4"
  }

  "infix chained" in {
    eval("1 + 2 + 3") shouldBe "6"
  }

  "infix with prefix" in {
    eval("sum 3 4 + 5") shouldBe "12"
  }

  "function arg consumes infix" in {
    // sqrt takes 1 arg, should get 16 (9 + 7), not just 9
    eval("sqrt 9 + 7") shouldBe "4"
  }

  "complex infix with function" in {
    // 5 * sqrt(16) = 5 * 4 = 20
    eval("5 * sqrt 9 + 7") shouldBe "20"
  }
