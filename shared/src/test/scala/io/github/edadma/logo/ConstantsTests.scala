package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.math.{E, Pi}

class ConstantsTests extends AnyFreeSpec with Matchers with Test:
  "constants 1" in {
    eval("pi") shouldBe Pi.toString
  }

  "constants 2" in {
    eval("e") shouldBe E.toString
  }
