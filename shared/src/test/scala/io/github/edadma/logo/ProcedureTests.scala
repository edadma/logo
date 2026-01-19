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

  // Optional parameters tests
  "procedure with optional param - use default" in {
    eval("""
      |to increment :x [:by 1]
      |  output :x + :by
      |end
      |(increment 5)
      |""".stripMargin) shouldBe "6"
  }

  "procedure with optional param - override default" in {
    eval("""
      |to increment :x [:by 1]
      |  output :x + :by
      |end
      |(increment 5 10)
      |""".stripMargin) shouldBe "15"
  }

  "procedure with multiple optional params - defaults" in {
    eval("""
      |to add3 :a [:b 0] [:c 0]
      |  output :a + :b + :c
      |end
      |(add3 5)
      |""".stripMargin) shouldBe "5"
  }

  "procedure with multiple optional params - some provided" in {
    eval("""
      |to add3 :a [:b 0] [:c 0]
      |  output :a + :b + :c
      |end
      |(add3 5 10)
      |""".stripMargin) shouldBe "15"
  }

  "procedure with multiple optional params - all provided" in {
    eval("""
      |to add3 :a [:b 0] [:c 0]
      |  output :a + :b + :c
      |end
      |(add3 5 10 20)
      |""".stripMargin) shouldBe "35"
  }

  // Rest parameter tests
  "procedure with rest param - returns list" in {
    eval("""
      |to mylist [:items]
      |  output :items
      |end
      |(mylist 1 2 3)
      |""".stripMargin) shouldBe "1 2 3"
  }

  "procedure with rest param - empty" in {
    eval("""
      |to mylist [:items]
      |  output :items
      |end
      |(mylist)
      |""".stripMargin) shouldBe ""
  }

  "procedure with required and rest params" in {
    eval("""
      |to prepend :first [:rest]
      |  output sentence :first :rest
      |end
      |(prepend 1 2 3 4)
      |""".stripMargin) shouldBe "1 2 3 4"
  }

  // CPS control flow tests
  "output in nested if" in {
    eval("""
      |to test :x
      |  if :x > 0 [
      |    if :x > 5 [output "big]
      |    output "small
      |  ]
      |  output "negative
      |end
      |test 10
      |""".stripMargin) shouldBe "big"
  }

  "stop in nested repeat" in {
    run("""
      |to test
      |  repeat 5 [
      |    print repcount
      |    if repcount = 3 [stop]
      |  ]
      |  print "done
      |end
      |test
      |""".stripMargin) shouldBe "1\n2\n3"
  }

  "output propagates through repeat" in {
    eval("""
      |to find :target
      |  repeat 10 [
      |    if repcount = :target [output repcount * 10]
      |  ]
      |  output 0
      |end
      |find 5
      |""".stripMargin) shouldBe "50"
  }

  "mutual recursion with output" in {
    eval("""
      |to iseven :n
      |  if :n = 0 [output true]
      |  output isodd :n - 1
      |end
      |to isodd :n
      |  if :n = 0 [output false]
      |  output iseven :n - 1
      |end
      |iseven 4
      |""".stripMargin) shouldBe "true"
  }

  "deep recursion countdown" in {
    run("""
      |to countdown :n
      |  if :n = 0 [print "done stop]
      |  countdown :n - 1
      |end
      |countdown 100
      |""".stripMargin) shouldBe "done"
  }

  "very deep recursion" in {
    // This tests TCO - would overflow stack without it
    run("""
      |to countdown :n
      |  if :n = 0 [print "done stop]
      |  countdown :n - 1
      |end
      |countdown 50000
      |""".stripMargin) shouldBe "done"
  }
