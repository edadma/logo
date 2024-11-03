package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.{include, startWith}
import org.scalatest.matchers.should.Matchers

class BasicTests extends AnyFreeSpec with Matchers with Test {

  "literal 1" in {
    eval("123") shouldBe "123"
  }

  "literal 2" in {
    an[Exception] should be thrownBy eval("123x")
  }
}
