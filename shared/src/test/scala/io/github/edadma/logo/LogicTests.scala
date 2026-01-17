package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LogicTests extends AnyFreeSpec with Matchers with Test:
  // Logical operators (prefix)
  "and true true" in {
    eval("and true true") shouldBe "true"
  }

  "and true false" in {
    eval("and true false") shouldBe "false"
  }

  "and false true" in {
    eval("and false true") shouldBe "false"
  }

  "or true false" in {
    eval("or true false") shouldBe "true"
  }

  "or false false" in {
    eval("or false false") shouldBe "false"
  }

  "not true" in {
    eval("not true") shouldBe "false"
  }

  "not false" in {
    eval("not false") shouldBe "true"
  }

  // Comparison predicates (prefix)
  "lessp true" in {
    eval("lessp 3 5") shouldBe "true"
  }

  "lessp false" in {
    eval("lessp 5 3") shouldBe "false"
  }

  "greaterp true" in {
    eval("greaterp 5 3") shouldBe "true"
  }

  "greaterp false" in {
    eval("greaterp 3 5") shouldBe "false"
  }

  "lessequalp equal" in {
    eval("lessequalp 5 5") shouldBe "true"
  }

  "lessequalp less" in {
    eval("lessequalp 3 5") shouldBe "true"
  }

  "greaterequalp equal" in {
    eval("greaterequalp 5 5") shouldBe "true"
  }

  "greaterequalp greater" in {
    eval("greaterequalp 5 3") shouldBe "true"
  }

  "notequalp true" in {
    eval("notequalp 3 5") shouldBe "true"
  }

  "notequalp false" in {
    eval("notequalp 5 5") shouldBe "false"
  }

  // Combined logic with comparisons
  "and with comparisons" in {
    eval("and lessp 3 5 greaterp 10 5") shouldBe "true"
  }

  "or with comparisons" in {
    eval("or lessp 5 3 greaterp 10 5") shouldBe "true"
  }

  "not with comparison" in {
    eval("not lessp 5 3") shouldBe "true"
  }

  // Logic in control flow - test that logical operators work with if
  "if with and true" in {
    // and returns true, so body executes and returns 1
    eval("if and true true [1]") shouldBe "1"
  }

  "if with or true" in {
    eval("if or false true [1]") shouldBe "1"
  }

  "if with not true" in {
    eval("if not false [1]") shouldBe "1"
  }
