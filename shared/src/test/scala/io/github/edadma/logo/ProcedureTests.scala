package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ProcedureTests extends AnyFreeSpec with Matchers with Test:
  "simple procedure no args" in {
    run("""
      |to greet
      |  print "hello
      |end
      |greet
      |""".stripMargin) shouldBe "hello"
  }

  "procedure with one arg" in {
    eval("""
      |to double :x
      |  output :x * 2
      |end
      |double 5
      |""".stripMargin) shouldBe "10"
  }

  "procedure with two args" in {
    eval("""
      |to add :a :b
      |  output :a + :b
      |end
      |add 3 4
      |""".stripMargin) shouldBe "7"
  }

  "procedure using op alias" in {
    eval("""
      |to square :n
      |  op :n * :n
      |end
      |square 5
      |""".stripMargin) shouldBe "25"
  }

  "procedure with stop" in {
    run("""
      |to test :x
      |  if :x < 0 [stop]
      |  print :x
      |end
      |test 5
      |test -1
      |test 3
      |""".stripMargin) shouldBe "5\n3"
  }

  "recursive procedure" in {
    eval("""
      |to factorial :n
      |  if :n < 2 [output 1]
      |  output :n * factorial :n - 1
      |end
      |factorial 5
      |""".stripMargin) shouldBe "120"
  }

  "procedure scope - params don't leak" in {
    eval("""
      |to test :x
      |  output :x + 1
      |end
      |make "x 100
      |test 5
      |:x
      |""".stripMargin) shouldBe "100"
  }

  "nested procedure calls" in {
    eval("""
      |to double :x
      |  output :x * 2
      |end
      |to quadruple :x
      |  output double double :x
      |end
      |quadruple 3
      |""".stripMargin) shouldBe "12"
  }
