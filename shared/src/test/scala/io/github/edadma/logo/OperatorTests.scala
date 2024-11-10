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
    eval("sum 3 4") shouldBe "7"
  }

  "arithmetic 6" in {
    eval("3 - 4") shouldBe "-1"
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

  "comparison 2" in {
    eval("5 = 5") shouldBe "true"
  }

  "comparison 3" in {
    eval("5 != 5") shouldBe "false"
  }

  "comparison 4" in {
    eval("3 + 4 < 5 * 6") shouldBe "true"
  }

  "comparison 5" in {
    eval("3 + 4 > 5 * 6") shouldBe "false"
  }
