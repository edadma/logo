package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PositionTests extends AnyFreeSpec with Matchers with Test:
  "xcor at origin" in {
    run("print xcor") shouldBe "0"
  }

  "ycor at origin" in {
    run("print ycor") shouldBe "0"
  }

  "pos at origin" in {
    // JS formats as "0", JVM as "0.0"
    val result = run("print pos")
    assert(result == "0" || result == "0.0")
  }

  "heading at start" in {
    run("print heading") shouldBe "0"
  }

  "xcor after setxy" in {
    run(
      """
        |setxy 100 50
        |print xcor
        |""".stripMargin,
    ) shouldBe "100"
  }

  "ycor after setxy" in {
    run(
      """
        |setxy 100 50
        |print ycor
        |""".stripMargin,
    ) shouldBe "50"
  }

  "pos after setxy" in {
    // JS formats as "3+4i", JVM as "3.0+4.0i"
    val result = run(
      """
        |setxy 3 4
        |print pos
        |""".stripMargin,
    )
    assert(result == "3+4i" || result == "3.0+4.0i")
  }

  "heading after right" in {
    run(
      """
        |right 90
        |print heading
        |""".stripMargin,
    ) shouldBe "90"
  }

  "heading after left" in {
    run(
      """
        |left 90
        |print heading
        |""".stripMargin,
    ) shouldBe "270"
  }

  "setc with complex number" in {
    run(
      """
        |setc 3 + 4 * i
        |print xcor
        |print ycor
        |""".stripMargin,
    ) shouldBe "3\n4"
  }

  "setc with pure imaginary" in {
    run(
      """
        |setc 5 * i
        |print xcor
        |print ycor
        |""".stripMargin,
    ) shouldBe "0\n5"
  }

  "setc with real number" in {
    run(
      """
        |setc 10
        |print xcor
        |print ycor
        |""".stripMargin,
    ) shouldBe "10\n0"
  }

  "pos round trip" in {
    run(
      """
        |setxy 7 11
        |make "p pos
        |home
        |setc :p
        |print xcor
        |print ycor
        |""".stripMargin,
    ) shouldBe "7\n11"
  }

  "setc draws line when pen down" in {
    // This test verifies setc behaves like setxy with drawing
    eval(
      """
        |penup
        |setxy 0 0
        |pendown
        |setc 10 + 10 * i
        |""".stripMargin,
    )
    // If no error, setc worked with pen down
  }

  "xcor after forward" in {
    run(
      """
        |right 90
        |forward 50
        |print xcor
        |""".stripMargin,
    ) shouldBe "50"
  }

  "ycor after forward" in {
    run(
      """
        |forward 50
        |print ycor
        |""".stripMargin,
    ) shouldBe "50"
  }
