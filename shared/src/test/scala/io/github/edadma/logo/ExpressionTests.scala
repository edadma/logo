package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ExpressionTests extends AnyFreeSpec with Matchers with Test:
  "expression 1" in {
    eval("( 123 )") shouldBe "123"
  }

  "expression 2" in {
    an[Exception] should be thrownBy eval("( 123")
  }

  "expression 3" in {
    an[Exception] should be thrownBy eval("123 )")
  }
