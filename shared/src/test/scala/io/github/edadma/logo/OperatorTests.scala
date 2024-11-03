package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class OperatorTests extends AnyFreeSpec with Matchers with Test {

  "arithmetic 1" in {
    eval("sum 3 4") shouldBe "7"
  }

  "arithmetic 2" in {
    an[Exception] should be thrownBy eval("sum 3 a")
  }
}
