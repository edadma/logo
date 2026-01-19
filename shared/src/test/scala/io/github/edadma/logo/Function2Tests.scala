package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Function2Tests extends AnyFreeSpec with Matchers with Test:
  "product" in {
    eval("product 3 4") shouldBe "12"
  }

  "quotient" in {
    eval("quotient 12 4") shouldBe "3"
  }

  "pow" in {
    eval("pow 2 3") shouldBe "8"
  }

  "pow fractional" in {
    eval("pow 4 0.5") shouldBe "2"
  }

  "pow negative exponent" in {
    eval("pow 2 -1") shouldBe "1/2" // DAL returns exact rational
  }

  // ============================================================================
  // atan2
  // ============================================================================

  "atan2 basic" in {
    eval("atan2 1 1") shouldBe (scala.math.Pi / 4).toString
  }

  "atan2 y=0 x=1" in {
    eval("atan2 0 1") shouldBe "0"
  }

  "atan2 y=1 x=0" in {
    eval("atan2 1 0") shouldBe (scala.math.Pi / 2).toString
  }
