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

  "floor positive" in {
    eval("floor 3.7") shouldBe "3"
  }

  "floor negative" in {
    eval("floor -3.2") shouldBe "-4"
  }

  "floor integer" in {
    eval("floor 5") shouldBe "5"
  }

  "ceiling positive" in {
    eval("ceiling 3.2") shouldBe "4"
  }

  "ceiling negative" in {
    eval("ceiling -3.7") shouldBe "-3"
  }

  "ceiling integer" in {
    eval("ceiling 5") shouldBe "5"
  }

  "ceil alias" in {
    eval("ceil 2.1") shouldBe "3"
  }

  "sign positive" in {
    eval("sign 42") shouldBe "1"
  }

  "sign negative" in {
    eval("sign -17") shouldBe "-1"
  }

  "sign zero" in {
    eval("sign 0") shouldBe "0"
  }

  "min two args" in {
    eval("min 5 3") shouldBe "3"
  }

  "min with negative" in {
    eval("min -10 5") shouldBe "-10"
  }

  "min variadic" in {
    eval("(min 5 3 8 1 9)") shouldBe "1"
  }

  "max two args" in {
    eval("max 5 3") shouldBe "5"
  }

  "max with negative" in {
    eval("max -10 5") shouldBe "5"
  }

  "max variadic" in {
    eval("(max 5 3 8 1 9)") shouldBe "9"
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
  // String Case: lowercase, uppercase
  // ============================================================================

  "lowercase basic" in {
    eval("lowercase \"HELLO") shouldBe "hello"
  }

  "lowercase mixed" in {
    eval("lowercase \"HeLLo") shouldBe "hello"
  }

  "lowercase already lower" in {
    eval("lowercase \"world") shouldBe "world"
  }

  "uppercase basic" in {
    eval("uppercase \"hello") shouldBe "HELLO"
  }

  "uppercase mixed" in {
    eval("uppercase \"HeLLo") shouldBe "HELLO"
  }

  "uppercase already upper" in {
    eval("uppercase \"WORLD") shouldBe "WORLD"
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

  // ============================================================================
  // Number Formatting: form
  // ============================================================================

  "form basic decimal" in {
    eval("form 3.14159 8 3") shouldBe "   3.142"
  }

  "form zero precision" in {
    eval("form 42 5 0") shouldBe "   42"
  }

  "form exact width" in {
    eval("form 3.14 4 2") shouldBe "3.14"
  }

  "form exceeds width" in {
    eval("form 12345.678 5 2") shouldBe "12345.68"
  }

  "form negative number" in {
    eval("form -3.14159 8 2") shouldBe "   -3.14"
  }

  "form zero" in {
    eval("form 0 5 2") shouldBe " 0.00"
  }

  "form large precision" in {
    eval("form 1.5 10 5") shouldBe "   1.50000"
  }

  "form integer as input" in {
    eval("form 42 6 2") shouldBe " 42.00"
  }

  "form small number" in {
    eval("form 0.001 8 4") shouldBe "  0.0010"
  }

  "form width 1" in {
    eval("form 5 1 0") shouldBe "5"
  }

  "form in expression" in {
    eval("""
      |make "formatted form 99.5 6 1
      |:formatted
    """.stripMargin) shouldBe "  99.5"
  }

  // ============================================================================
  // ignore - discard a value
  // ============================================================================

  "ignore discards value" in {
    run("""
      |ignore 42
      |print "done
    """.stripMargin) shouldBe "done"
  }

  "ignore with expression" in {
    run("""
      |ignore 3 + 4
      |print "done
    """.stripMargin) shouldBe "done"
  }

  "ignore with procedure output" in {
    run("""
      |to double :x
      |  output :x * 2
      |end
      |ignore double 5
      |print "done
    """.stripMargin) shouldBe "done"
  }

  "ignore returns null" in {
    // ignore should not output anything, so using it in expression context should give null
    eval("ignore 42") shouldBe "null"
  }

  // ============================================================================
  // parse - convert text to token list
  // ============================================================================

  "parse simple word" in {
    eval("parse \"hello") shouldBe "hello"
  }

  "parse numbers" in {
    eval("parse \"42") shouldBe "42"
  }

  "parse returns list" in {
    eval("listp parse \"hello") shouldBe "true"
  }

  "parse multiple tokens" in {
    eval("count parse [hello world]") shouldBe "2"
  }

  "parse list from word" in {
    run("print parse [1 2 3]") shouldBe "1 2 3"
  }

  // ============================================================================
  // runparse - parse with variable substitution
  // ============================================================================

  "runparse simple" in {
    eval("runparse \"hello") shouldBe "hello"
  }

  "runparse substitutes variable" in {
    run("""
      |make "x 42
      |print first runparse ":x
    """.stripMargin) shouldBe "42"
  }

  "runparse with list" in {
    run("""
      |make "x 10
      |make "y 20
      |print runparse [:x :y]
    """.stripMargin) shouldBe "10 20"
  }

  "runparse returns list" in {
    eval("listp runparse \"hello") shouldBe "true"
  }

  // ============================================================================
  // runresult - run code and wrap output in list
  // ============================================================================

  "runresult with output" in {
    eval("runresult [output 42]") shouldBe "42"
  }

  "runresult wraps in list" in {
    eval("count runresult [output 42]") shouldBe "1"
  }

  "runresult no output gives empty list" in {
    eval("count runresult [print 42]") shouldBe "0"
  }

  "runresult with procedure" in {
    run("""
      |to double :x
      |  output :x * 2
      |end
      |print first runresult [double 5]
    """.stripMargin) shouldBe "10"
  }

  "runresult empty list for stop" in {
    run("""
      |to myproc
      |  print "hello
      |  stop
      |end
      |print count runresult [myproc]
    """.stripMargin) shouldBe "hello\n0"
  }

  "runresult with expression output" in {
    eval("first runresult [output 3 + 4]") shouldBe "7"
  }

  "runresult nested" in {
    eval("first runresult [output first runresult [output 99]]") shouldBe "99"
  }

  "runresult preserves outer context" in {
    run("""
      |to outer
      |  make "result runresult [output 42]
      |  output first :result
      |end
      |print outer
    """.stripMargin) shouldBe "42"
  }

  "runresult in arithmetic" in {
    run("""
      |make "result runresult [output 42]
      |print (first :result) + 1
    """.stripMargin) shouldBe "43"
  }
