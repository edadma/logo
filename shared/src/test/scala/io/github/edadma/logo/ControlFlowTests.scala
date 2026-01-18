package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ControlFlowTests extends AnyFreeSpec with Matchers with Test:
  "repeat" in {
    run(
      """
        |make "x 0
        |repeat 5 [make "x sum x 1]
        |print x
        |""".stripMargin,
    ) shouldBe "5"
  }

  "repeat nested" in {
    run(
      """
        |make "x 0
        |repeat 3 [repeat 4 [make "x sum x 1]]
        |print x
        |""".stripMargin,
    ) shouldBe "12"
  }

  "if true" in {
    run(
      """
        |make "x 0
        |if true [make "x 1]
        |print x
        |""".stripMargin,
    ) shouldBe "1"
  }

  "if false" in {
    run(
      """
        |make "x 0
        |if false [make "x 1]
        |print x
        |""".stripMargin,
    ) shouldBe "0"
  }

  "if with equalp" in {
    run(
      """
        |make "x 5
        |if equalp x 5 [make "x 10]
        |print x
        |""".stripMargin,
    ) shouldBe "10"
  }

  "ifelse true branch" in {
    run(
      """
        |ifelse true [print 1] [print 2]
        |""".stripMargin,
    ) shouldBe "1"
  }

  "ifelse false branch" in {
    run(
      """
        |ifelse false [print 1] [print 2]
        |""".stripMargin,
    ) shouldBe "2"
  }

  "ifelse with condition" in {
    run(
      """
        |make "x 10
        |ifelse equalp x 10 [print "yes] [print "no]
        |""".stripMargin,
    ) shouldBe "yes"
  }

  "repcount basic" in {
    run(
      """
        |repeat 5 [print repcount]
        |""".stripMargin,
    ) shouldBe "1\n2\n3\n4\n5"
  }

  "repcount in expression" in {
    run(
      """
        |make "sum 0
        |repeat 4 [make "sum :sum + repcount]
        |print :sum
        |""".stripMargin,
    ) shouldBe "10"
  }

  "repcount nested inner" in {
    run(
      """
        |repeat 2 [
        |  repeat 3 [print repcount]
        |]
        |""".stripMargin,
    ) shouldBe "1\n2\n3\n1\n2\n3"
  }

  "repcount nested outer" in {
    run(
      """
        |repeat 3 [
        |  make "outer repcount
        |  repeat 2 [print :outer]
        |]
        |""".stripMargin,
    ) shouldBe "1\n1\n2\n2\n3\n3"
  }

  "repcount outside repeat" in {
    run("print repcount") shouldBe "0"
  }
