package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BuiltinTests extends AnyFreeSpec with Matchers with Test:

  // ============================================================================
  // Numeric Functions: abs, int, round
  // ============================================================================

  "abs of positive" in {
    eval("abs 5") shouldBe "5"
  }

  "abs of negative" in {
    eval("abs -5") shouldBe "5"
  }

  "abs of zero" in {
    eval("abs 0") shouldBe "0"
  }

  "abs of decimal" in {
    eval("abs -3.7") shouldBe "3.7"
  }

  "int truncates positive" in {
    eval("int 3.7") shouldBe "3"
  }

  "int truncates negative" in {
    eval("int -3.7") shouldBe "-3"
  }

  "int of integer" in {
    eval("int 5") shouldBe "5"
  }

  "round up" in {
    eval("round 3.7") shouldBe "4"
  }

  "round down" in {
    eval("round 3.2") shouldBe "3"
  }

  "round half" in {
    eval("round 3.5") shouldBe "4"
  }

  "round negative" in {
    eval("round -3.7") shouldBe "-4"
  }

  // ============================================================================
  // Character/ASCII: ascii, char
  // ============================================================================

  "ascii of letter" in {
    eval("ascii \"A") shouldBe "65"
  }

  "ascii of lowercase" in {
    eval("ascii \"a") shouldBe "97"
  }

  "ascii of digit" in {
    eval("ascii \"0") shouldBe "48"
  }

  "ascii of space" in {
    eval("ascii char 32") shouldBe "32"
  }

  "char of 65" in {
    eval("char 65") shouldBe "A"
  }

  "char of 97" in {
    eval("char 97") shouldBe "a"
  }

  "char of 48" in {
    eval("char 48") shouldBe "0"
  }

  "ascii char roundtrip" in {
    eval("char ascii \"X") shouldBe "X"
  }

  // ============================================================================
  // List/Word Operations: reverse, pick
  // ============================================================================

  "reverse list" in {
    eval("reverse [1 2 3 4 5]") shouldBe "5 4 3 2 1"
  }

  "reverse word" in {
    eval("reverse \"hello") shouldBe "olleh"
  }

  "reverse empty list" in {
    eval("reverse []") shouldBe ""
  }

  "reverse single element" in {
    eval("reverse [a]") shouldBe "a"
  }

  "reverse single char" in {
    eval("reverse \"x") shouldBe "x"
  }

  "pick from list returns element" in {
    val result = run("""
      |make "x pick [a b c]
      |print memberp :x [a b c]
    """.stripMargin)
    result shouldBe "true"
  }

  "pick from word returns char" in {
    val result = run("""
      |make "x pick "abc
      |print memberp :x "abc
    """.stripMargin)
    result shouldBe "true"
  }

  // ============================================================================
  // Variable Access: thing
  // ============================================================================

  "thing gets variable" in {
    val result = run("""
      |make "x 42
      |print thing "x
    """.stripMargin)
    result shouldBe "42"
  }

  "thing with word value" in {
    val result = run("""
      |make "greeting "hello
      |print thing "greeting
    """.stripMargin)
    result shouldBe "hello"
  }

  "thing with list value" in {
    val result = run("""
      |make "mylist [1 2 3]
      |print thing "mylist
    """.stripMargin)
    result shouldBe "1 2 3"
  }

  "thing with dynamic name" in {
    val result = run("""
      |make "varname "foo
      |make "foo 99
      |print thing :varname
    """.stripMargin)
    result shouldBe "99"
  }

  // ============================================================================
  // Output: type, show
  // ============================================================================

  "type without newline" in {
    val result = run("""
      |type "hello
      |print "world
    """.stripMargin)
    result shouldBe "helloworld"
  }

  "type multiple args" in {
    val result = run("""
      |(type "a "b "c)
    """.stripMargin)
    result shouldBe "a b c"
  }

  "type with space" in {
    val result = run("""
      |type "hello
      |type char 32
      |print "world
    """.stripMargin)
    result shouldBe "hello world"
  }

  "show quotes words" in {
    val result = run("""
      |show "hello
    """.stripMargin)
    result shouldBe "\"hello"
  }

  "show formats list" in {
    val result = run("""
      |show [a b c]
    """.stripMargin)
    result shouldBe "[a b c]"
  }

  "show number unchanged" in {
    val result = run("""
      |show 42
    """.stripMargin)
    result shouldBe "42"
  }

  // ============================================================================
  // Turtle: setx, sety, setheading
  // ============================================================================

  "setx changes x only" in {
    val result = run("""
      |setxy 10 20
      |setx 50
      |print xcor
      |print ycor
    """.stripMargin)
    result shouldBe "50\n20"
  }

  "sety changes y only" in {
    val result = run("""
      |setxy 10 20
      |sety 50
      |print xcor
      |print ycor
    """.stripMargin)
    result shouldBe "10\n50"
  }

  "setheading sets direction" in {
    val result = run("""
      |setheading 90
      |print heading
    """.stripMargin)
    result shouldBe "90"
  }

  "seth alias" in {
    val result = run("""
      |seth 180
      |print heading
    """.stripMargin)
    result shouldBe "180"
  }

  "setheading 0 is north" in {
    val result = run("""
      |setheading 0
      |print heading
    """.stripMargin)
    result shouldBe "0"
  }

  "setheading 270" in {
    val result = run("""
      |setheading 270
      |print heading
    """.stripMargin)
    result shouldBe "270"
  }

  // ============================================================================
  // Synonym: mod
  // ============================================================================

  "mod synonym for remainder" in {
    eval("mod 17 5") shouldBe "2"
  }

  "mod with negative" in {
    eval("mod -17 5") shouldBe "-2"
  }
