package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.math.*

class Function1Tests extends AnyFreeSpec with Matchers with Test:
  // Trig functions (UCB Logo: degrees)
  "sin" in {
    eval("sin 0").toDouble shouldBe 0.0
  }

  "sin 90 degrees" in {
    eval("sin 90").toDouble shouldBe (1.0 +- 0.0001)
  }

  "cos" in {
    eval("cos 0").toDouble shouldBe (1.0 +- 0.0001)
  }

  "cos 180 degrees" in {
    eval("cos 180").toDouble shouldBe (-1.0 +- 0.0001)
  }

  "tan" in {
    eval("tan 0").toDouble shouldBe 0.0
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

  // ============================================================================
  // Inverse Trigonometric Functions
  // ============================================================================

  // Inverse trig now returns degrees (UCB Logo standard)
  "asin 0" in {
    eval("asin 0").toDouble shouldBe 0.0
  }

  "asin 1 returns 90 degrees" in {
    eval("asin 1").toDouble shouldBe (90.0 +- 0.0001)
  }

  "asin -1 returns -90 degrees" in {
    eval("asin -1").toDouble shouldBe (-90.0 +- 0.0001)
  }

  "arcsin alias" in {
    eval("arcsin 0").toDouble shouldBe 0.0
  }

  "asin of imaginary i" in {
    val result = eval("asin i")
    result should include("i")
  }

  "asin auto-promotes to complex when |x| > 1" in {
    val result = eval("asin 2")
    result should include("i") // should return complex, not NaN
  }

  "acos 1 returns 0 degrees" in {
    eval("acos 1").toDouble shouldBe 0.0
  }

  "acos 0 returns 90 degrees" in {
    eval("acos 0").toDouble shouldBe (90.0 +- 0.0001)
  }

  "acos -1 returns 180 degrees" in {
    eval("acos -1").toDouble shouldBe (180.0 +- 0.0001)
  }

  "arccos alias" in {
    eval("arccos 1").toDouble shouldBe 0.0
  }

  "acos of imaginary i" in {
    val result = eval("acos i")
    result should include("i")
  }

  "acos auto-promotes to complex when |x| > 1" in {
    val result = eval("acos 2")
    result should include("i") // should return complex, not NaN
  }

  "atan 0" in {
    eval("atan 0").toDouble shouldBe 0.0
  }

  "atan 1 returns 45 degrees" in {
    eval("atan 1").toDouble shouldBe (45.0 +- 0.0001)
  }

  "arctan alias" in {
    eval("arctan 0").toDouble shouldBe 0.0
  }

  "atan of imaginary i/2" in {
    val result = eval("atan (i / 2)")
    result should include("i")
  }

  // ============================================================================
  // Logarithms
  // ============================================================================

  "log10 1" in {
    eval("log10 1") shouldBe "0"
  }

  "log10 10" in {
    eval("log10 10") shouldBe "1"
  }

  "log10 100" in {
    eval("log10 100") shouldBe "2"
  }

  "log10 1000" in {
    eval("log10 1000").toDouble shouldBe 3.0 +- 1e-10
  }
