package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.math.{E, Pi}

class Function0Tests extends AnyFreeSpec with Matchers with Test:
  "pi" in {
    eval("pi") shouldBe Pi.toString
  }

  "e" in {
    eval("e") shouldBe E.toString
  }
