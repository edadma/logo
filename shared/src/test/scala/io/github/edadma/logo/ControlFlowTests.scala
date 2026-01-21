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

  // ============================================================================
  // output ifelse - properly resolves pending ifelse
  // ============================================================================

  "output ifelse true" in {
    val result = run("""
      |to mytest :x
      |  output ifelse :x > 5 ["big] ["small]
      |end
      |print mytest 10
    """.stripMargin)
    result shouldBe "big"
  }

  "output ifelse false" in {
    val result = run("""
      |to mytest :x
      |  output ifelse :x > 5 ["big] ["small]
      |end
      |print mytest 3
    """.stripMargin)
    result shouldBe "small"
  }

  "output ifelse with expression result" in {
    val result = run("""
      |to double :x
      |  output ifelse :x > 0 [:x * 2] [0]
      |end
      |print double 5
      |print double -3
    """.stripMargin)
    result shouldBe "10\n0"
  }

  "nested output ifelse" in {
    val result = run("""
      |to grade :score
      |  output ifelse :score >= 90 ["A] [ifelse :score >= 80 ["B] [ifelse :score >= 70 ["C] ["F]]]
      |end
      |print grade 95
      |print grade 85
      |print grade 75
      |print grade 50
    """.stripMargin)
    result shouldBe "A\nB\nC\nF"
  }

  // ============================================================================
  // for - C-style for loop
  // ============================================================================

  "for basic ascending" in {
    run("""
      |for [i 1 5] [print :i]
    """.stripMargin) shouldBe "1\n2\n3\n4\n5"
  }

  "for basic descending" in {
    run("""
      |for [i 5 1] [print :i]
    """.stripMargin) shouldBe "5\n4\n3\n2\n1"
  }

  "for with explicit step" in {
    run("""
      |for [i 1 10 2] [print :i]
    """.stripMargin) shouldBe "1\n3\n5\n7\n9"
  }

  "for with negative step" in {
    run("""
      |for [i 10 1 -2] [print :i]
    """.stripMargin) shouldBe "10\n8\n6\n4\n2"
  }

  "for accumulator" in {
    run("""
      |make "sum 0
      |for [i 1 5] [make "sum :sum + :i]
      |print :sum
    """.stripMargin) shouldBe "15"
  }

  "for nested" in {
    run("""
      |for [i 1 2] [
      |  for [j 1 3] [
      |    print :i * 10 + :j
      |  ]
      |]
    """.stripMargin) shouldBe "11\n12\n13\n21\n22\n23"
  }

  "for variable scope" in {
    run("""
      |make "i 100
      |for [i 1 3] [print :i]
      |print :i
    """.stripMargin) shouldBe "1\n2\n3\n100"
  }

  "for with expressions" in {
    run("""
      |make "start 2
      |make "end 6
      |for [i :start :end] [print :i]
    """.stripMargin) shouldBe "2\n3\n4\n5\n6"
  }

  "for empty range" in {
    run("""
      |for [i 5 1 1] [print :i]
      |print "done
    """.stripMargin) shouldBe "done"
  }

  "for with stop" in {
    run("""
      |to mytest
      |  for [i 1 10] [
      |    print :i
      |    if :i = 3 [stop]
      |  ]
      |  print "after
      |end
      |mytest
    """.stripMargin) shouldBe "1\n2\n3"
  }

  "for with output" in {
    val result = run("""
      |to findFirst :target
      |  for [i 1 10] [
      |    if :i = :target [output :i * 10]
      |  ]
      |  output 0
      |end
      |print findFirst 5
    """.stripMargin)
    result shouldBe "50"
  }

  // ============================================================================
  // while - loop while condition is true
  // ============================================================================

  "while basic" in {
    run("""
      |make "x 1
      |while [:x < 5] [
      |  print :x
      |  make "x :x + 1
      |]
    """.stripMargin) shouldBe "1\n2\n3\n4"
  }

  "while never executes" in {
    run("""
      |make "x 10
      |while [:x < 5] [print :x]
      |print "done
    """.stripMargin) shouldBe "done"
  }

  "while countdown" in {
    run("""
      |make "x 5
      |while [:x > 0] [
      |  print :x
      |  make "x :x - 1
      |]
    """.stripMargin) shouldBe "5\n4\n3\n2\n1"
  }

  "while with complex condition" in {
    run("""
      |make "x 0
      |make "y 10
      |while [:x < :y] [
      |  make "x :x + 1
      |  make "y :y - 1
      |]
      |print :x
      |print :y
    """.stripMargin) shouldBe "5\n5"
  }

  "while with stop" in {
    run("""
      |to mytest
      |  make "x 1
      |  while [true] [
      |    print :x
      |    make "x :x + 1
      |    if :x > 3 [stop]
      |  ]
      |  print "after
      |end
      |mytest
    """.stripMargin) shouldBe "1\n2\n3"
  }

  "while with output" in {
    run("""
      |to findPower2 :target
      |  make "x 1
      |  while [:x < :target] [
      |    make "x :x * 2
      |  ]
      |  output :x
      |end
      |print findPower2 100
    """.stripMargin) shouldBe "128"
  }

  // ============================================================================
  // until - loop while condition is false
  // ============================================================================

  "until basic" in {
    run("""
      |make "x 1
      |until [:x >= 5] [
      |  print :x
      |  make "x :x + 1
      |]
    """.stripMargin) shouldBe "1\n2\n3\n4"
  }

  "until countdown" in {
    run("""
      |make "x 5
      |until [:x = 0] [
      |  print :x
      |  make "x :x - 1
      |]
    """.stripMargin) shouldBe "5\n4\n3\n2\n1"
  }

  // ============================================================================
  // setitem - destructive list update
  // ============================================================================

  "setitem basic" in {
    run("""
      |make "mylist [a b c d e]
      |setitem 3 "mylist "X
      |print :mylist
    """.stripMargin) shouldBe "a b X d e"
  }

  "setitem first" in {
    run("""
      |make "mylist [1 2 3]
      |setitem 1 "mylist 100
      |print :mylist
    """.stripMargin) shouldBe "100 2 3"
  }

  "setitem last" in {
    run("""
      |make "mylist [1 2 3]
      |setitem 3 "mylist 100
      |print :mylist
    """.stripMargin) shouldBe "1 2 100"
  }

  "setitem in loop" in {
    run("""
      |make "mylist [0 0 0 0 0]
      |for [i 1 5] [
      |  setitem :i "mylist :i * :i
      |]
      |print :mylist
    """.stripMargin) shouldBe "1 4 9 16 25"
  }

  // ============================================================================
  // push/pop - stack operations
  // ============================================================================

  "push basic" in {
    run("""
      |make "stack []
      |push "stack 1
      |push "stack 2
      |push "stack 3
      |print :stack
    """.stripMargin) shouldBe "3 2 1"
  }

  "pop basic" in {
    run("""
      |make "stack [3 2 1]
      |print pop "stack
      |print pop "stack
      |print :stack
    """.stripMargin) shouldBe "3\n2\n1"
  }

  "push and pop" in {
    run("""
      |make "stack []
      |push "stack "a
      |push "stack "b
      |print pop "stack
      |push "stack "c
      |print pop "stack
      |print pop "stack
    """.stripMargin) shouldBe "b\nc\na"
  }

  // ============================================================================
  // queue/dequeue - queue operations
  // ============================================================================

  "queue basic" in {
    run("""
      |make "q []
      |queue "q 1
      |queue "q 2
      |queue "q 3
      |print :q
    """.stripMargin) shouldBe "1 2 3"
  }

  "dequeue basic" in {
    run("""
      |make "q [1 2 3]
      |print dequeue "q
      |print dequeue "q
      |print :q
    """.stripMargin) shouldBe "1\n2\n3"
  }

  "queue and dequeue FIFO" in {
    run("""
      |make "q []
      |queue "q "a
      |queue "q "b
      |print dequeue "q
      |queue "q "c
      |print dequeue "q
      |print dequeue "q
    """.stripMargin) shouldBe "a\nb\nc"
  }

  // ============================================================================
  // Deep recursion tests for for/while (TCO verification)
  // ============================================================================

  "for deep iteration" in {
    run("""
      |make "sum 0
      |for [i 1 10000] [make "sum :sum + 1]
      |print :sum
    """.stripMargin) shouldBe "10000"
  }

  "while deep iteration" in {
    run("""
      |make "x 0
      |while [:x < 10000] [make "x :x + 1]
      |print :x
    """.stripMargin) shouldBe "10000"
  }

  // ============================================================================
  // do.while - executes body first, then checks condition
  // ============================================================================

  "do.while basic" in {
    run("""
      |make "x 0
      |do.while [make "x :x + 1 print :x] [:x < 3]
    """.stripMargin) shouldBe "1\n2\n3"
  }

  "do.while executes at least once" in {
    run("""
      |make "x 10
      |do.while [print :x make "x :x + 1] [:x < 5]
    """.stripMargin) shouldBe "10"
  }

  "do.while with accumulator" in {
    run("""
      |make "sum 0
      |make "i 1
      |do.while [
      |  make "sum :sum + :i
      |  make "i :i + 1
      |] [:i <= 5]
      |print :sum
    """.stripMargin) shouldBe "15"
  }

  "do.while countdown" in {
    run("""
      |make "x 3
      |do.while [print :x make "x :x - 1] [:x > 0]
    """.stripMargin) shouldBe "3\n2\n1"
  }

  "do.while with stop" in {
    run("""
      |to mytest
      |  make "x 0
      |  do.while [
      |    make "x :x + 1
      |    print :x
      |    if :x = 2 [stop]
      |  ] [:x < 5]
      |  print "after
      |end
      |mytest
    """.stripMargin) shouldBe "1\n2"
  }

  "do.while deep iteration" in {
    run("""
      |make "x 0
      |do.while [make "x :x + 1] [:x < 10000]
      |print :x
    """.stripMargin) shouldBe "10000"
  }

  // ============================================================================
  // do.until - executes body first, then loops while condition is false
  // ============================================================================

  "do.until basic" in {
    run("""
      |make "x 0
      |do.until [make "x :x + 1 print :x] [:x >= 3]
    """.stripMargin) shouldBe "1\n2\n3"
  }

  "do.until executes at least once" in {
    run("""
      |make "x 10
      |do.until [print :x make "x :x + 1] [:x > 5]
    """.stripMargin) shouldBe "10"
  }

  "do.until countdown" in {
    run("""
      |make "x 3
      |do.until [print :x make "x :x - 1] [:x = 0]
    """.stripMargin) shouldBe "3\n2\n1"
  }

  "do.until with equality" in {
    run("""
      |make "x 0
      |do.until [make "x :x + 1] [:x = 5]
      |print :x
    """.stripMargin) shouldBe "5"
  }

  // ============================================================================
  // test/iftrue/iffalse - flag-based conditionals
  // ============================================================================

  "test iftrue basic" in {
    run("""
      |test 5 > 3
      |iftrue [print "yes]
    """.stripMargin) shouldBe "yes"
  }

  "test iffalse basic" in {
    run("""
      |test 3 > 5
      |iffalse [print "no]
    """.stripMargin) shouldBe "no"
  }

  "test iftrue iffalse together" in {
    run("""
      |test 10 > 5
      |iftrue [print "bigger]
      |iffalse [print "smaller]
    """.stripMargin) shouldBe "bigger"
  }

  "test iftrue iffalse false case" in {
    run("""
      |test 3 > 5
      |iftrue [print "bigger]
      |iffalse [print "smaller]
    """.stripMargin) shouldBe "smaller"
  }

  "ift alias" in {
    run("""
      |test true
      |ift [print "works]
    """.stripMargin) shouldBe "works"
  }

  "iff alias" in {
    run("""
      |test false
      |iff [print "works]
    """.stripMargin) shouldBe "works"
  }

  "test can be updated" in {
    run("""
      |test true
      |iftrue [print "first]
      |test false
      |iftrue [print "second]
      |iffalse [print "third]
    """.stripMargin) shouldBe "first\nthird"
  }

  "test with expression" in {
    run("""
      |make "x 10
      |test :x > 5
      |iftrue [print "big]
      |test :x < 5
      |iftrue [print "small]
      |iffalse [print "not-small]
    """.stripMargin) shouldBe "big\nnot-small"
  }

  "test in procedure" in {
    run("""
      |to check :n
      |  test :n > 0
      |  iftrue [print "positive]
      |  iffalse [print "non-positive]
      |end
      |check 5
      |check -3
    """.stripMargin) shouldBe "positive\nnon-positive"
  }

  "test with equalp" in {
    run("""
      |test equalp 5 5
      |iftrue [print "equal]
    """.stripMargin) shouldBe "equal"
  }

  // ============================================================================
  // case - multi-way branching
  // ============================================================================

  "case basic match first" in {
    run("""
      |make "x 1
      |case :x [[[1] print "one] [[2] print "two] [[3] print "three]]
    """.stripMargin) shouldBe "one"
  }

  "case basic match middle" in {
    run("""
      |make "x 2
      |case :x [[[1] print "one] [[2] print "two] [[3] print "three]]
    """.stripMargin) shouldBe "two"
  }

  "case basic match last" in {
    run("""
      |make "x 3
      |case :x [[[1] print "one] [[2] print "two] [[3] print "three]]
    """.stripMargin) shouldBe "three"
  }

  "case else clause" in {
    run("""
      |make "x 5
      |case :x [[[1] print "one] [[2] print "two] [else print "other]]
    """.stripMargin) shouldBe "other"
  }

  "case no match no else" in {
    run("""
      |make "x 99
      |case :x [[[1] print "one] [[2] print "two]]
      |print "done
    """.stripMargin) shouldBe "done"
  }

  "case multiple values in selector" in {
    run("""
      |make "x 2
      |case :x [[[1 2 3] print "small] [[4 5 6] print "medium] [else print "large]]
    """.stripMargin) shouldBe "small"
  }

  "case multiple values match any" in {
    run("""
      |make "x 5
      |case :x [[[1 2 3] print "small] [[4 5 6] print "medium] [else print "large]]
    """.stripMargin) shouldBe "medium"
  }

  "case with words" in {
    run("""
      |make "color "red
      |case :color [[[red] print "hot] [[blue] print "cold] [else print "neutral]]
    """.stripMargin) shouldBe "hot"
  }

  "case with word list" in {
    run("""
      |make "fruit "apple
      |case :fruit [[[apple banana orange] print "common] [else print "exotic]]
    """.stripMargin) shouldBe "common"
  }

  "case with multiple statements in body" in {
    run("""
      |make "x 1
      |case :x [[[1] print "one print "uno] [[2] print "two]]
    """.stripMargin) shouldBe "one\nuno"
  }

  "case in procedure" in {
    run("""
      |to describe :n
      |  case :n [[[1] print "one] [[2] print "two] [else print "many]]
      |end
      |describe 1
      |describe 2
      |describe 100
    """.stripMargin) shouldBe "one\ntwo\nmany"
  }

  "case with expression" in {
    run("""
      |make "x 3
      |make "y 2
      |case :x + :y [[[4] print "four] [[5] print "five] [[6] print "six]]
    """.stripMargin) shouldBe "five"
  }

  "case else case-insensitive" in {
    run("""
      |make "x 99
      |case :x [[[1] print "one] [ELSE print "other]]
    """.stripMargin) shouldBe "other"
  }

  // ============================================================================
  // cond - conditional expressions
  // ============================================================================

  "cond basic first true" in {
    run("""
      |make "x 5
      |cond [[[:x < 10] print "small] [[:x < 20] print "medium] [else print "large]]
    """.stripMargin) shouldBe "small"
  }

  "cond basic second true" in {
    run("""
      |make "x 15
      |cond [[[:x < 10] print "small] [[:x < 20] print "medium] [else print "large]]
    """.stripMargin) shouldBe "medium"
  }

  "cond else clause" in {
    run("""
      |make "x 100
      |cond [[[:x < 10] print "small] [[:x < 20] print "medium] [else print "large]]
    """.stripMargin) shouldBe "large"
  }

  "cond no match no else" in {
    run("""
      |make "x 100
      |cond [[[:x < 10] print "small] [[:x < 20] print "medium]]
      |print "done
    """.stripMargin) shouldBe "done"
  }

  "cond with equality" in {
    run("""
      |make "x 5
      |cond [[[:x = 5] print "five] [[:x = 10] print "ten] [else print "other]]
    """.stripMargin) shouldBe "five"
  }

  "cond with complex condition" in {
    run("""
      |make "x 15
      |make "y 20
      |cond [[[and :x > 10 :y > 15] print "both-big] [else print "not-both]]
    """.stripMargin) shouldBe "both-big"
  }

  "cond with multiple statements" in {
    run("""
      |make "x 5
      |cond [[[:x < 10] print "small print "tiny] [else print "big]]
    """.stripMargin) shouldBe "small\ntiny"
  }

  "cond in procedure" in {
    run("""
      |to categorize :n
      |  cond [[[:n < 0] print "negative] [[:n = 0] print "zero] [else print "positive]]
      |end
      |categorize -5
      |categorize 0
      |categorize 10
    """.stripMargin) shouldBe "negative\nzero\npositive"
  }

  "cond short-circuits" in {
    run("""
      |make "x 5
      |cond [[[true] print "first] [[true] print "second]]
    """.stripMargin) shouldBe "first"
  }

  "cond else case-insensitive" in {
    run("""
      |make "x 100
      |cond [[[:x < 10] print "small] [ELSE print "other]]
    """.stripMargin) shouldBe "other"
  }

  "cond with greaterp" in {
    run("""
      |make "x 10
      |cond [[[greaterp :x 5] print "big] [else print "small]]
    """.stripMargin) shouldBe "big"
  }

  "cond with nested condition" in {
    run("""
      |make "a 5
      |make "b 10
      |cond [[[:a + :b > 12] print "sum-big] [[:a * :b > 40] print "product-big] [else print "both-small]]
    """.stripMargin) shouldBe "sum-big"
  }
