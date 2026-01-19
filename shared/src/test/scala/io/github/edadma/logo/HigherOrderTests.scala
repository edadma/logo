package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class HigherOrderTests extends AnyFreeSpec with Matchers with Test:

  // ============================================================================
  // APPLY - call procedure with list of arguments
  // ============================================================================

  "apply with word template" - {
    "apply sum to list of numbers" in {
      eval("apply \"sum [1 2 3]") shouldBe "6"
    }

    "apply product to list" in {
      eval("apply \"product [2 3 4]") shouldBe "24"
    }

    "apply word to two args" in {
      eval("apply \"word [hel lo]") shouldBe "hello"
    }

    "apply list (fput)" in {
      eval("apply \"fput [a [b c]]") shouldBe "a b c"
    }

    "apply user-defined procedure" in {
      val result = run("""
        |to double :x
        |  output :x * 2
        |end
        |print apply "double [5]
      """.stripMargin)
      result shouldBe "10"
    }
  }

  "apply with list template" - {
    "apply template with ?1 and ?2" in {
      eval("apply [?1 + ?2] [3 4]") shouldBe "7"
    }

    "apply template with ?1 ?2 ?3" in {
      eval("apply [?1 + ?2 + ?3] [1 2 3]") shouldBe "6"
    }

    "apply comparison template" in {
      eval("apply [?1 > ?2] [5 3]") shouldBe "true"
    }
  }

  // ============================================================================
  // INVOKE - call procedure by name with list of arguments
  // ============================================================================

  "invoke" - {
    "invoke sum" in {
      eval("invoke \"sum [10 20]") shouldBe "30"
    }

    "invoke with variable procedure name" in {
      val result = run("""
        |make "op "sum
        |print invoke :op [5 10 15]
      """.stripMargin)
      result shouldBe "30"
    }
  }

  // ============================================================================
  // FOREACH - apply template for side effects
  // ============================================================================

  "foreach" - {
    "foreach with print" in {
      val result = run("""
        |foreach [print ?] [a b c]
      """.stripMargin)
      result shouldBe "a\nb\nc"
    }

    "foreach with word template" in {
      val result = run("""
        |foreach "print [1 2 3]
      """.stripMargin)
      result shouldBe "1\n2\n3"
    }

    "foreach with user procedure" in {
      val result = run("""
        |to showit :x
        |  print word "item: :x
        |end
        |foreach "showit [a b]
      """.stripMargin)
      result shouldBe "item:a\nitem:b"
    }

    "foreach empty list does nothing" in {
      val result = run("""
        |foreach [print ?] []
        |print "done
      """.stripMargin)
      result shouldBe "done"
    }
  }

  // ============================================================================
  // MAP - transform each element
  // ============================================================================

  "map with list template" - {
    "map double" in {
      eval("map [? * 2] [1 2 3 4]") shouldBe "2 4 6 8"
    }

    "map square" in {
      eval("map [? * ?] [1 2 3 4]") shouldBe "1 4 9 16"
    }

    "map with addition" in {
      eval("map [? + 10] [1 2 3]") shouldBe "11 12 13"
    }

    "map with nested expression" in {
      eval("map [? * ? + 1] [1 2 3]") shouldBe "2 5 10"
    }

    "map preserves order" in {
      eval("map [? * 3] [5 4 3 2 1]") shouldBe "15 12 9 6 3"
    }
  }

  "map with word template" - {
    "map sqrt" in {
      eval("map \"sqrt [4 9 16 25]") shouldBe "2 3 4 5"
    }

    "map abs" in {
      eval("map \"abs [-3 -2 -1 0 1 2 3]") shouldBe "3 2 1 0 1 2 3"
    }

    "map first" in {
      eval("map \"first [[a b] [c d] [e f]]") shouldBe "a c e"
    }

    "map butfirst returns list of lists" in {
      // map returns a list of results, not flattened
      val result = run("print count map \"butfirst [[a b c] [d e f]]")
      result shouldBe "2"
    }

    "map count" in {
      eval("map \"count [[a] [b c] [d e f]]") shouldBe "1 2 3"
    }

    "map user procedure" in {
      val result = run("""
        |to triple :n
        |  output :n * 3
        |end
        |print map "triple [1 2 3 4]
      """.stripMargin)
      result shouldBe "3 6 9 12"
    }
  }

  "map edge cases" - {
    "map empty list" in {
      eval("map [? * 2] []") shouldBe ""
    }

    "map single element" in {
      eval("map [? + 1] [5]") shouldBe "6"
    }

    "map with words" in {
      eval("map \"uppercase [hello world]") shouldBe "HELLO WORLD"
    }
  }

  // ============================================================================
  // MAP.SE - map with sentence (flatten results)
  // ============================================================================

  "map.se" - {
    "map.se with list results" in {
      eval("map.se [list ? ?] [1 2 3]") shouldBe "1 1 2 2 3 3"
    }

    "map.se flattens nested lists" in {
      val result = run("""
        |to pair :x
        |  output list :x :x
        |end
        |print map.se "pair [a b c]
      """.stripMargin)
      result shouldBe "a a b b c c"
    }

    "map.se with word results (no flattening needed)" in {
      // first of a single-element list returns that element
      eval("map.se \"first [[ab] [cd] [ef]]") shouldBe "ab cd ef"
    }

    "map.se empty list" in {
      eval("map.se [list ? ?] []") shouldBe ""
    }
  }

  // ============================================================================
  // FILTER - keep elements matching predicate
  // ============================================================================

  "filter with list template" - {
    "filter greater than" in {
      eval("filter [? > 5] [3 6 2 8 1 9 4]") shouldBe "6 8 9"
    }

    "filter less than" in {
      eval("filter [? < 5] [3 6 2 8 1 9 4]") shouldBe "3 2 1 4"
    }

    "filter equal" in {
      eval("filter [? = 3] [1 2 3 3 4 3 5]") shouldBe "3 3 3"
    }

    "filter with complex condition" in {
      eval("filter [? > 2] filter [? < 8] [1 2 3 4 5 6 7 8 9]") shouldBe "3 4 5 6 7"
    }

    "filter even numbers (using modulo)" in {
      eval("filter [0 = modulo ? 2] [1 2 3 4 5 6 7 8]") shouldBe "2 4 6 8"
    }

    "filter odd numbers" in {
      eval("filter [1 = modulo ? 2] [1 2 3 4 5 6 7 8]") shouldBe "1 3 5 7"
    }
  }

  "filter with word template" - {
    "filter numberp" in {
      eval("filter \"numberp [1 a 2 b 3 c]") shouldBe "1 2 3"
    }

    "filter wordp" in {
      eval("filter \"wordp [1 a 2 b 3 c]") shouldBe "a b c"
    }

    "filter user predicate" in {
      val result = run("""
        |to bigp :n
        |  output :n > 10
        |end
        |print filter "bigp [5 15 8 20 3 12]
      """.stripMargin)
      result shouldBe "15 20 12"
    }
  }

  "filter edge cases" - {
    "filter all match" in {
      eval("filter [? > 0] [1 2 3]") shouldBe "1 2 3"
    }

    "filter none match" in {
      eval("filter [? > 100] [1 2 3]") shouldBe ""
    }

    "filter empty list" in {
      eval("filter [? > 0] []") shouldBe ""
    }

    "filter single element match" in {
      eval("filter [? = 5] [5]") shouldBe "5"
    }

    "filter single element no match" in {
      eval("filter [? = 5] [3]") shouldBe ""
    }
  }

  // ============================================================================
  // FIND - first element matching predicate
  // ============================================================================

  "find" - {
    "find first greater than" in {
      eval("find [? > 5] [3 6 2 8 1]") shouldBe "6"
    }

    "find first equal" in {
      eval("find [? = 3] [1 2 3 4 5]") shouldBe "3"
    }

    "find returns first match only" in {
      eval("find [? > 0] [1 2 3 4 5]") shouldBe "1"
    }

    "find with word template" in {
      eval("find \"numberp [a b 3 c 4]") shouldBe "3"
    }

    "find not found returns empty list" in {
      eval("find [? > 100] [1 2 3]") shouldBe ""
    }

    "find in empty list" in {
      eval("find [? > 0] []") shouldBe ""
    }

    "find user predicate" in {
      val result = run("""
        |to evenp :n
        |  output 0 = modulo :n 2
        |end
        |print find "evenp [1 3 5 6 7 8]
      """.stripMargin)
      result shouldBe "6"
    }
  }

  // ============================================================================
  // REDUCE - fold with binary operation
  // ============================================================================

  "reduce with list template" - {
    "reduce sum" in {
      eval("reduce [?1 + ?2] [1 2 3 4 5]") shouldBe "15"
    }

    "reduce product" in {
      eval("reduce [?1 * ?2] [1 2 3 4 5]") shouldBe "120"
    }

    "reduce max" in {
      eval("reduce [ifelse ?1 > ?2 [?1] [?2]] [3 7 2 9 1]") shouldBe "9"
    }

    "reduce min" in {
      eval("reduce [ifelse ?1 < ?2 [?1] [?2]] [3 7 2 9 1]") shouldBe "1"
    }

    "reduce with words (concatenate)" in {
      eval("reduce [word ?1 ?2] [a b c d]") shouldBe "abcd"
    }

    "reduce single element" in {
      eval("reduce [?1 + ?2] [42]") shouldBe "42"
    }
  }

  "reduce with word template" - {
    "reduce sum" in {
      eval("reduce \"sum [1 2 3 4]") shouldBe "10"
    }

    "reduce product" in {
      eval("reduce \"product [2 3 4]") shouldBe "24"
    }

    "reduce word" in {
      eval("reduce \"word [h e l l o]") shouldBe "hello"
    }

    "reduce user procedure" in {
      // Note: using if/output pattern instead of output ifelse to avoid pending resolution issue
      val result = run("""
        |to bigger :a :b
        |  if :a > :b [output :a]
        |  output :b
        |end
        |print reduce "bigger [5 2 8 3 1]
      """.stripMargin)
      result shouldBe "8"
    }
  }

  // ============================================================================
  // Composition and Complex Cases
  // ============================================================================

  "composition" - {
    "map then filter" in {
      eval("filter [? > 10] map [? * 2] [3 4 5 6 7]") shouldBe "12 14"
    }

    "filter then map" in {
      eval("map [? * 2] filter [? > 3] [1 2 3 4 5]") shouldBe "8 10"
    }

    "filter then reduce" in {
      eval("reduce [?1 + ?2] filter [? > 2] [1 2 3 4 5]") shouldBe "12"
    }

    "map then reduce" in {
      eval("reduce [?1 + ?2] map [? * ?] [1 2 3 4]") shouldBe "30"
    }

    "sum of squares of even numbers" in {
      eval("reduce [?1 + ?2] map [? * ?] filter [0 = modulo ? 2] [1 2 3 4 5 6]") shouldBe "56"
    }
  }

  "nested templates" - {
    "map with list operation" in {
      eval("map [first ?] [[a b] [c d] [e f]]") shouldBe "a c e"
    }

    "map with conditional" in {
      eval("map [ifelse ? > 5 [\"big] [\"small]] [3 7 2 9]") shouldBe "small big small big"
    }
  }
