package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ListTests extends AnyFreeSpec with Matchers with Test:

  // first
  "first of list" in {
    eval("first [a b c]") shouldBe "a"
  }

  "first of word" in {
    eval("first \"hello") shouldBe "h"
  }

  "first of single element list" in {
    eval("first [x]") shouldBe "x"
  }

  // last
  "last of list" in {
    eval("last [a b c]") shouldBe "c"
  }

  "last of word" in {
    eval("last \"hello") shouldBe "o"
  }

  "last of single element list" in {
    eval("last [x]") shouldBe "x"
  }

  "last of single character word" in {
    eval("last \"a") shouldBe "a"
  }

  // butfirst
  "butfirst of list" in {
    eval("butfirst [a b c]") shouldBe "b c"
  }

  "bf alias" in {
    eval("bf [1 2 3]") shouldBe "2 3"
  }

  "butfirst of word" in {
    eval("butfirst \"hello") shouldBe "ello"
  }

  "butfirst to single element" in {
    eval("butfirst [a b]") shouldBe "b"
  }

  "butfirst to empty" in {
    eval("butfirst [a]") shouldBe ""
  }

  // butlast
  "butlast of list" in {
    eval("butlast [a b c]") shouldBe "a b"
  }

  "bl alias" in {
    eval("bl [1 2 3]") shouldBe "1 2"
  }

  "butlast of word" in {
    eval("butlast \"hello") shouldBe "hell"
  }

  "butlast to empty" in {
    eval("butlast [a]") shouldBe ""
  }

  // fput
  "fput element to list" in {
    eval("fput 1 [2 3]") shouldBe "1 2 3"
  }

  "fput to empty list" in {
    eval("fput \"a []") shouldBe "a"
  }

  // lput
  "lput element to list" in {
    eval("lput 3 [1 2]") shouldBe "1 2 3"
  }

  "lput to empty list" in {
    eval("lput \"z []") shouldBe "z"
  }

  // item
  "item from list" in {
    eval("item 2 [a b c]") shouldBe "b"
  }

  "item from word" in {
    eval("item 3 \"hello") shouldBe "l"
  }

  "item first element" in {
    eval("item 1 [x y z]") shouldBe "x"
  }

  // count
  "count of list" in {
    eval("count [a b c d]") shouldBe "4"
  }

  "count of word" in {
    eval("count \"hello") shouldBe "5"
  }

  "count of empty list" in {
    eval("count []") shouldBe "0"
  }

  // emptyp
  "emptyp true for empty list" in {
    eval("emptyp []") shouldBe "true"
  }

  "emptyp false for non-empty list" in {
    eval("emptyp [a]") shouldBe "false"
  }

  "empty? alias" in {
    eval("empty? []") shouldBe "true"
  }

  "emptyp for word" in {
    eval("emptyp \"hello") shouldBe "false"
  }

  // listp
  "listp true for list" in {
    eval("listp [a b]") shouldBe "true"
  }

  "listp false for word" in {
    eval("listp \"hello") shouldBe "false"
  }

  "list? alias" in {
    eval("list? [1 2 3]") shouldBe "true"
  }

  // wordp
  "wordp true for word" in {
    eval("wordp \"hello") shouldBe "true"
  }

  "wordp false for list" in {
    eval("wordp [a b]") shouldBe "false"
  }

  "word? alias" in {
    eval("word? \"test") shouldBe "true"
  }

  // numberp
  "numberp true for number" in {
    eval("numberp 42") shouldBe "true"
  }

  "numberp false for word" in {
    eval("numberp \"hello") shouldBe "false"
  }

  "number? alias" in {
    eval("number? 3.14") shouldBe "true"
  }

  // memberp
  "memberp true when element in list" in {
    eval("memberp \"b [a b c]") shouldBe "true"
  }

  "memberp false when element not in list" in {
    eval("memberp \"x [a b c]") shouldBe "false"
  }

  "memberp for character in word" in {
    eval("memberp \"e \"hello") shouldBe "true"
  }

  "member? alias" in {
    eval("member? \"x [a x b]") shouldBe "true"
  }

  // Combination tests
  "first of butfirst" in {
    eval("first butfirst [a b c]") shouldBe "b"
  }

  "count of butfirst" in {
    eval("count butfirst [a b c d]") shouldBe "3"
  }

  "fput first to butfirst" in {
    eval("fput first [a b c] butfirst [a b c]") shouldBe "a b c"
  }

  // ============================================================================
  // Range: generate integer sequences
  // ============================================================================

  "range single arg" in {
    eval("range 5") shouldBe "0 1 2 3 4"
  }

  "range two args" in {
    eval("(range 1 5)") shouldBe "1 2 3 4"
  }

  "range with step" in {
    eval("(range 0 10 2)") shouldBe "0 2 4 6 8"
  }

  "range negative step" in {
    eval("(range 5 0 -1)") shouldBe "5 4 3 2 1"
  }

  "range empty" in {
    eval("range 0") shouldBe ""
  }

  "range start equals end" in {
    eval("(range 5 5)") shouldBe ""
  }

  "iseq alias" in {
    eval("(iseq 1 4)") shouldBe "1 2 3"
  }
