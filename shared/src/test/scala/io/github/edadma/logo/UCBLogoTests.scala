package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class UCBLogoTests extends AnyFreeSpec with Matchers with Test:

  // ============================================================================
  // Trigonometry: degrees (UCB Logo standard)
  // ============================================================================

  "sin of 0 degrees" in {
    eval("sin 0").toDouble shouldBe 0.0
  }

  "sin of 90 degrees" in {
    eval("sin 90").toDouble shouldBe (1.0 +- 0.0001)
  }

  "sin of 30 degrees" in {
    eval("sin 30").toDouble shouldBe (0.5 +- 0.0001)
  }

  "cos of 0 degrees" in {
    eval("cos 0").toDouble shouldBe (1.0 +- 0.0001)
  }

  "cos of 90 degrees" in {
    eval("cos 90").toDouble shouldBe (0.0 +- 0.0001)
  }

  "cos of 60 degrees" in {
    eval("cos 60").toDouble shouldBe (0.5 +- 0.0001)
  }

  "tan of 45 degrees" in {
    eval("tan 45").toDouble shouldBe (1.0 +- 0.0001)
  }

  "asin returns degrees" in {
    val result = eval("asin 0.5").toDouble
    result shouldBe (30.0 +- 0.0001)
  }

  "acos returns degrees" in {
    val result = eval("acos 0.5").toDouble
    result shouldBe (60.0 +- 0.0001)
  }

  "atan returns degrees" in {
    val result = eval("atan 1").toDouble
    result shouldBe (45.0 +- 0.0001)
  }

  "atan2 returns degrees" in {
    val result = eval("atan2 1 1").toDouble
    result shouldBe (45.0 +- 0.0001)
  }

  // ============================================================================
  // Radian versions
  // ============================================================================

  "radsin uses radians" in {
    val result = eval("radsin 1.5707963267948966").toDouble // pi/2
    result shouldBe (1.0 +- 0.0001)
  }

  "radcos uses radians" in {
    val result = eval("radcos 0").toDouble
    result shouldBe (1.0 +- 0.0001)
  }

  "radtan uses radians" in {
    val result = eval("radtan 0.7853981633974483").toDouble // pi/4
    result shouldBe (1.0 +- 0.0001)
  }

  "radarcsin returns radians" in {
    val result = eval("radarcsin 1").toDouble
    result shouldBe (1.5707963267948966 +- 0.0001) // pi/2
  }

  "radarccos returns radians" in {
    val result = eval("radarccos 0").toDouble
    result shouldBe (1.5707963267948966 +- 0.0001) // pi/2
  }

  "radarctan returns radians" in {
    val result = eval("radarctan 1").toDouble
    result shouldBe (0.7853981633974483 +- 0.0001) // pi/4
  }

  // ============================================================================
  // Modulo vs Remainder
  // ============================================================================

  "remainder positive" in {
    eval("remainder 17 5") shouldBe "2"
  }

  "remainder negative dividend" in {
    eval("remainder -17 5") shouldBe "-2"
  }

  "modulo positive" in {
    eval("modulo 17 5").toDouble shouldBe 2.0
  }

  "modulo negative dividend gives positive result" in {
    eval("modulo -17 5").toDouble shouldBe 3.0
  }

  "modulo negative divisor" in {
    eval("modulo 17 -5").toDouble shouldBe -3.0
  }

  "modulo both negative" in {
    eval("modulo -17 -5").toDouble shouldBe -2.0
  }

  // ============================================================================
  // Bitwise Operations
  // ============================================================================

  "bitand" in {
    eval("bitand 12 10") shouldBe "8"
  }

  "bitor" in {
    eval("bitor 12 10") shouldBe "14"
  }

  "bitxor" in {
    eval("bitxor 12 10") shouldBe "6"
  }

  "bitnot" in {
    eval("bitnot 0") shouldBe "-1"
  }

  "bitnot of 1" in {
    eval("bitnot 1") shouldBe "-2"
  }

  "ashift left" in {
    eval("ashift 1 4") shouldBe "16"
  }

  "ashift right" in {
    eval("ashift 16 -2") shouldBe "4"
  }

  "ashift right preserves sign" in {
    eval("ashift -16 -2") shouldBe "-4"
  }

  "lshift left" in {
    eval("lshift 1 4") shouldBe "16"
  }

  "lshift right" in {
    eval("lshift 16 -2") shouldBe "4"
  }

  // ============================================================================
  // List Operations: member, remove, remdup
  // ============================================================================

  "member found in list" in {
    eval("member 3 [1 2 3 4 5]") shouldBe "3 4 5"
  }

  "member not found returns empty" in {
    eval("member 9 [1 2 3]") shouldBe ""
  }

  "member in word" in {
    eval("member \"c \"abcde") shouldBe "cde"
  }

  "member not found in word" in {
    eval("member \"z \"abcde") shouldBe ""
  }

  "remove from list" in {
    eval("remove 2 [1 2 3 2 4]") shouldBe "1 3 4"
  }

  "remove from word" in {
    eval("remove \"a \"banana") shouldBe "bnn"
  }

  "remdup list" in {
    eval("remdup [1 2 1 3 2 4]") shouldBe "1 2 3 4"
  }

  "remdup word" in {
    eval("remdup \"mississippi") shouldBe "misp"
  }

  // ============================================================================
  // List Operations: combine, firsts, butfirsts
  // ============================================================================

  "combine words" in {
    eval("combine \"hel \"lo") shouldBe "hello"
  }

  "combine to list (fput)" in {
    eval("combine 1 [2 3]") shouldBe "1 2 3"
  }

  "firsts" in {
    eval("firsts [[1 2] [3 4] [5 6]]") shouldBe "1 3 5"
  }

  "firsts with words" in {
    // firsts extracts first of each word/list element
    val result = run("""
      |make "words [abc def ghi]
      |print firsts :words
    """.stripMargin)
    result shouldBe "a d g"
  }

  "butfirsts returns list of butfirsts" in {
    // butfirsts returns [[2 3] [5 6]], each butfirst wrapped as its own element
    val result = run("""
      |print count butfirsts [[1 2 3] [4 5 6]]
    """.stripMargin)
    result shouldBe "2"
  }

  "bfs alias" in {
    val result = run("""
      |print count bfs [[1 2] [3 4]]
    """.stripMargin)
    result shouldBe "2"
  }

  "butfirsts with words" in {
    val result = run("""
      |make "words [abc def]
      |print butfirsts :words
    """.stripMargin)
    result shouldBe "bc ef"
  }

  // ============================================================================
  // Predicates: beforep, substringp
  // ============================================================================

  "beforep true" in {
    eval("beforep \"abc \"def") shouldBe "true"
  }

  "beforep false" in {
    eval("beforep \"xyz \"abc") shouldBe "false"
  }

  "beforep equal" in {
    eval("beforep \"abc \"abc") shouldBe "false"
  }

  "before? alias" in {
    eval("before? \"a \"b") shouldBe "true"
  }

  "substringp true" in {
    eval("substringp \"bc \"abcd") shouldBe "true"
  }

  "substringp false" in {
    eval("substringp \"xy \"abcd") shouldBe "false"
  }

  "substring? alias" in {
    eval("substring? \"sub \"substring") shouldBe "true"
  }

  // ============================================================================
  // Utilities: gensym, quoted, rerandom, rseq
  // ============================================================================

  "gensym generates unique symbols" in {
    val result = run("""
      |print gensym
      |print gensym
      |print gensym
    """.stripMargin)
    val symbols = result.split("\n")
    symbols.length shouldBe 3
    symbols.distinct.length shouldBe 3 // all unique
    symbols.forall(_.startsWith("G")) shouldBe true
  }

  "quoted prepends quote" in {
    eval("quoted \"hello") shouldBe "\"hello"
  }

  "quoted with number" in {
    eval("quoted 42") shouldBe "\"42"
  }

  "rerandom resets generator" in {
    // Test that rerandom with same seed produces same sequence
    val result = run("""
      |rerandom 12345
      |make "a1 int random 1000000
      |make "a2 int random 1000000
      |rerandom 12345
      |make "b1 int random 1000000
      |make "b2 int random 1000000
      |print :a1
      |print :b1
    """.stripMargin)
    val lines = result.split("\n")
    lines(0) shouldBe lines(1)
  }

  "rseq basic" in {
    val result = run("print count rseq 0 10 3")
    result shouldBe "3"
  }

  "rseq single element" in {
    eval("rseq 5 10 1").toDouble shouldBe 5.0
  }

  "rseq with decimals" in {
    val result = run("print count rseq 0 1 5")
    result shouldBe "5"
  }

  "rseq values" in {
    val result = run("""
      |make "s rseq 0 1 5
      |print first :s
      |print last :s
    """.stripMargin)
    result shouldBe "0\n1"
  }

  // ============================================================================
  // Aliases
  // ============================================================================

  "minus alias for negate" in {
    eval("minus 5") shouldBe "-5"
  }

  "minus negative" in {
    eval("minus -3") shouldBe "3"
  }

  "equal? alias" in {
    eval("equal? 5 5") shouldBe "true"
  }

  "notequal? alias" in {
    eval("notequal? 5 3") shouldBe "true"
  }

  "less? alias" in {
    eval("less? 3 5") shouldBe "true"
  }

  "greater? alias" in {
    eval("greater? 5 3") shouldBe "true"
  }

  "lessequal? alias" in {
    eval("lessequal? 5 5") shouldBe "true"
  }

  "greaterequal? alias" in {
    eval("greaterequal? 5 5") shouldBe "true"
  }
