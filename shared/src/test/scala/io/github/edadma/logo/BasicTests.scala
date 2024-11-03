package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BasicTests extends AnyFreeSpec with Matchers with Test {

  "literal 1" in {
    eval("123") shouldBe "123"
  }

  "literal 2" in {
    eval("\"123x") shouldBe "123x"
  }

  "literal 3" in {
    an[Exception] should be thrownBy eval("123x")
  }

  "literal 4" in {
    eval("[a b]") shouldBe "a b"
  }

  "literal 5" in {
    eval("[a [b] c]") shouldBe "a [b] c"
  }

  "print 1" in {
    run("print 123") shouldBe "123"
  }

  "print 2" in {
    run("print \"123x") shouldBe "123x"
  }

  "print 3" in {
    an[Exception] should be thrownBy run("print 123x")
  }
}
