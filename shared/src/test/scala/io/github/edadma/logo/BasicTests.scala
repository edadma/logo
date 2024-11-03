package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BasicTests extends AnyFreeSpec with Matchers with Test {

  "literal 1" in {
    eval("123") shouldBe "123"
  }

  "literal 2" in {
    an[Exception] should be thrownBy eval("123x")
  }

  "print 1" in {
    run("print 123") shouldBe "123"
  }

  "print 2" in {
    run("print \"123x") shouldBe "123x"
  }
}
