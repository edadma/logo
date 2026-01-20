package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TurtleGraphicsTests extends AnyFreeSpec with Matchers with Test:
  // towards tests
  "towards point to the right" in {
    eval("towards [100 0]") shouldBe "90"
  }

  "towards point ahead" in {
    eval("towards [0 100]") shouldBe "0"
  }

  "towards point to the left" in {
    eval("towards [-100 0]") shouldBe "270"
  }

  "towards point behind" in {
    eval("towards [0 -100]") shouldBe "180"
  }

  "towards diagonal" in {
    eval("towards [100 100]") shouldBe "45"
  }

  "towards from offset position" in {
    eval("setxy 50 50 towards [100 50]") shouldBe "90"
  }

  // distance tests
  "distance to point on x-axis" in {
    eval("distance [100 0]") shouldBe "100"
  }

  "distance to point on y-axis" in {
    eval("distance [0 50]") shouldBe "50"
  }

  "distance to diagonal point" in {
    eval("distance [3 4]") shouldBe "5"
  }

  "distance from offset position" in {
    eval("setxy 10 10 distance [13 14]") shouldBe "5"
  }

  // screen mode tests
  "screenmode default is window" in {
    eval("screenmode") shouldBe "window"
  }

  "fence mode" in {
    eval("fence screenmode") shouldBe "fence"
  }

  "wrap mode" in {
    eval("wrap screenmode") shouldBe "wrap"
  }

  "window mode" in {
    eval("fence window screenmode") shouldBe "window"
  }

  "fence mode throws on boundary violation" in {
    an[Exception] should be thrownBy eval("fence forward 1000")
  }

  // pendownp and shownp tests
  "pendownp default is true" in {
    eval("pendownp") shouldBe "true"
  }

  "pendownp after penup" in {
    eval("penup pendownp") shouldBe "false"
  }

  "pendown? synonym" in {
    eval("pendown?") shouldBe "true"
  }

  "shownp default is true" in {
    eval("shownp") shouldBe "true"
  }

  "shownp after hideturtle" in {
    eval("hideturtle shownp") shouldBe "false"
  }

  "shown? synonym" in {
    eval("shown?") shouldBe "true"
  }

  // arc tests (basic - just ensure it doesn't error)
  "arc draws without error" in {
    noException should be thrownBy eval("arc 90 50")
  }

  "arc negative angle" in {
    noException should be thrownBy eval("arc -90 50")
  }

  "arc full circle" in {
    noException should be thrownBy eval("arc 360 100")
  }
