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

  // Power operator tests
  "power infix" in {
    eval("2 ^ 3") shouldBe "8"
  }

  "power precedence over multiplication" in {
    eval("2 * 3 ^ 2") shouldBe "18" // 2 * 9 = 18, not (2*3)^2 = 36
  }

  "power right associative" in {
    eval("2 ^ 3 ^ 2") shouldBe "512" // 2^(3^2) = 2^9 = 512, not (2^3)^2 = 64
  }

  // Comparison operator tests
  "infix equal true" in {
    eval("5 = 5") shouldBe "true"
  }

  "infix equal false" in {
    eval("5 = 6") shouldBe "false"
  }

  "infix not equal" in {
    eval("5 <> 6") shouldBe "true"
  }

  "infix less than true" in {
    eval("3 < 5") shouldBe "true"
  }

  "infix less than false" in {
    eval("5 < 3") shouldBe "false"
  }

  "infix greater than" in {
    eval("5 > 3") shouldBe "true"
  }

  "infix less equal" in {
    eval("5 <= 5") shouldBe "true"
  }

  "infix greater equal" in {
    eval("5 >= 6") shouldBe "false"
  }

  "comparison with arithmetic" in {
    eval("2 + 3 = 5") shouldBe "true"
  }

  "comparison precedence" in {
    eval("2 * 3 > 5") shouldBe "true" // 6 > 5
  }

  // Additional edge cases
  "unary minus in multiplication" in {
    eval("3 * -2") shouldBe "-6"
  }

  "negative base power" in {
    eval("-2 ^ 2") shouldBe "4"
  }

  "variable in infix with colon" in {
    eval("make \"x 10 :x * 2") shouldBe "20"
  }

  "variable in infix without colon" in {
    eval("make \"y 5 y + 3") shouldBe "8"
  }

  "chained multiplication" in {
    eval("2 * 3 * 4") shouldBe "24"
  }

  "chained division" in {
    eval("24 / 4 / 2") shouldBe "3"
  }

  "mixed add sub" in {
    eval("10 - 3 + 2") shouldBe "9"
  }

  "comparison both sides arithmetic" in {
    eval("2 + 3 < 4 + 5") shouldBe "true"
  }

  "power in comparison" in {
    eval("2 ^ 3 = 8") shouldBe "true"
  }

  "complex expression" in {
    eval("2 + 3 * 4 ^ 2 - 10 / 2") shouldBe "45" // 2 + 3*16 - 5 = 2 + 48 - 5 = 45
  }
