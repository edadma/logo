package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BasicTests extends AnyFreeSpec with Matchers with Test:
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

  "make 1" in {
    run(
      """
        |make "a 123
        |print a
        |""".stripMargin,
    ) shouldBe "123"
  }

  "make 2" in {
    run(
      """
        |make "a 123
        |print a
        |make "a 456
        |print a
        |""".stripMargin,
    ) shouldBe "123\n456"
  }

  "make 3" in {
    run(
      """
        |make "a 123
        |make "a sum a 5
        |print a
        |""".stripMargin,
    ) shouldBe "128"
  }

  // Colon syntax for variable references
  "colon variable reference" in {
    eval("make \"x 42 :x") shouldBe "42"
  }

  "colon variable in expression" in {
    eval("make \"x 10 :x + 5") shouldBe "15"
  }

  "colon variable shadows builtin" in {
    // :sum gets the variable, not the builtin procedure
    eval("make \"sum 99 :sum") shouldBe "99"
  }

  "colon undefined variable error" in {
    an[Exception] should be thrownBy eval(":undefined")
  }
