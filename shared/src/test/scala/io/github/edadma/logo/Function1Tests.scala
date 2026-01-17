package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.math.*

class Function1Tests extends AnyFreeSpec with Matchers with Test:
  // Trig functions
  "sin" in {
    eval("sin 0") shouldBe "0"
  }

  "sin pi/2" in {
    eval("sin pi / 2") shouldBe "1"
  }

  "cos" in {
    eval("cos 0") shouldBe "1"
  }

  "cos pi" in {
    eval("cos pi") shouldBe "-1"
  }

  "tan" in {
    eval("tan 0") shouldBe "0"
  }

  // Hyperbolic functions
  "sinh" in {
    eval("sinh 0") shouldBe "0"
  }

  "cosh" in {
    eval("cosh 0") shouldBe "1"
  }

  "tanh" in {
    eval("tanh 0") shouldBe "0"
  }

  // Other math functions
  "sqrt" in {
    eval("sqrt 16") shouldBe "4"
  }

  "sqrt 2" in {
    eval("sqrt 2") shouldBe sqrt(2).toString
  }

  "exp" in {
    eval("exp 0") shouldBe "1"
  }

  "exp 1" in {
    eval("exp 1") shouldBe E.toString
  }

  "ln" in {
    eval("ln 1") shouldBe "0"
  }

  "ln e" in {
    eval("ln e") shouldBe "1"
  }

  // random returns value in [0, limit)
  "random" in {
    val result = eval("random 10").toDouble
    result should be >= 0.0
    result should be < 10.0
  }
