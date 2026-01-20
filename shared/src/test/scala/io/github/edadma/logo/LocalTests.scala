package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LocalTests extends AnyFreeSpec with Matchers with Test:
  // local tests
  "local declares variable local to procedure" in {
    run(
      """
        |make "x 10
        |to test
        |  local "x
        |  make "x 20
        |  print :x
        |end
        |test
        |print :x
        |""".stripMargin
    ) shouldBe "20\n10"
  }

  "local with initially undefined variable" in {
    run(
      """
        |to test
        |  local "y
        |  make "y 5
        |  print :y
        |end
        |test
        |print namep "y
        |""".stripMargin
    ) shouldBe "5\nfalse"
  }

  "local multiple variables" in {
    run(
      """
        |make "a 1
        |make "b 2
        |to test
        |  local "a
        |  local "b
        |  make "a 100
        |  make "b 200
        |  print :a + :b
        |end
        |test
        |print :a + :b
        |""".stripMargin
    ) shouldBe "300\n3"
  }

  // localmake tests
  "localmake declares and assigns in one step" in {
    run(
      """
        |make "x 10
        |to test
        |  localmake "x 20
        |  print :x
        |end
        |test
        |print :x
        |""".stripMargin
    ) shouldBe "20\n10"
  }

  "localmake with initially undefined variable" in {
    run(
      """
        |to test
        |  localmake "z 42
        |  print :z
        |end
        |test
        |print namep "z
        |""".stripMargin
    ) shouldBe "42\nfalse"
  }

  "localmake in recursive procedure" in {
    run(
      """
        |to factorial :n
        |  localmake "result 1
        |  if :n < 2 [output :result]
        |  output :n * factorial :n - 1
        |end
        |print factorial 5
        |""".stripMargin
    ) shouldBe "120"
  }

  "nested procedures with local variables" in {
    run(
      """
        |make "x 0
        |to outer
        |  localmake "x 1
        |  inner
        |  print :x
        |end
        |to inner
        |  localmake "x 2
        |  print :x
        |end
        |outer
        |print :x
        |""".stripMargin
    ) shouldBe "2\n1\n0"
  }

  "local does not affect outer scope until procedure exits" in {
    run(
      """
        |make "count 0
        |to increment
        |  localmake "count :count + 1
        |  print :count
        |end
        |increment
        |increment
        |increment
        |print :count
        |""".stripMargin
    ) shouldBe "1\n1\n1\n0"
  }

  "declaring same variable local twice has no extra effect" in {
    run(
      """
        |make "x 10
        |to test
        |  local "x
        |  make "x 1
        |  local "x
        |  make "x 2
        |  print :x
        |end
        |test
        |print :x
        |""".stripMargin
    ) shouldBe "2\n10"
  }

  "localmake with quadratic formula expression" in {
    run(
      """
        |to quadratic :a :b :c
        |  localmake "x1 (-:b + sqrt (:b*:b - 4*:a*:c))/(2*:a)
        |  localmake "x2 (-:b - sqrt (:b*:b - 4*:a*:c))/(2*:a)
        |  print :x1
        |  print :x2
        |end
        |quadratic 1 -5 6
        |""".stripMargin
    ) shouldBe "3\n2"
  }
