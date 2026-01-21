package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TCOTests extends AnyFreeSpec with Matchers with Test:

  // ============================================================================
  // Basic Tail Recursion Tests
  // ============================================================================

  "tail recursive factorial" in {
    val result = run("""
      |to factorial :n
      |  output facthelper :n 1
      |end
      |
      |to facthelper :n :acc
      |  if :n = 0 [output :acc]
      |  output facthelper :n - 1 :acc * :n
      |end
      |
      |print factorial 10
    """.stripMargin)
    result shouldBe "3628800"
  }

  "tail recursive sum" in {
    val result = run("""
      |to sumto :n
      |  output sumhelper :n 0
      |end
      |
      |to sumhelper :n :acc
      |  if :n = 0 [output :acc]
      |  output sumhelper :n - 1 :acc + :n
      |end
      |
      |print sumto 100
    """.stripMargin)
    result shouldBe "5050"
  }

  "tail recursive countdown" in {
    val result = run("""
      |to countdown :n
      |  if :n = 0 [output "done]
      |  output countdown :n - 1
      |end
      |
      |print countdown 10
    """.stripMargin)
    result shouldBe "done"
  }

  // ============================================================================
  // Deep Recursion (would stack overflow without TCO)
  // ============================================================================

  "deep tail recursion does not stack overflow" in {
    val result = run("""
      |to deep :n
      |  if :n = 0 [output "done]
      |  output deep :n - 1
      |end
      |
      |print deep 50000
    """.stripMargin)
    result shouldBe "done"
  }

  "deep mutual recursion" in {
    val result = run("""
      |to evenp :n
      |  if :n = 0 [output true]
      |  output oddp :n - 1
      |end
      |
      |to oddp :n
      |  if :n = 0 [output false]
      |  output evenp :n - 1
      |end
      |
      |print evenp 1000
    """.stripMargin)
    result shouldBe "true"
  }

  // ============================================================================
  // Tail Position in Control Structures
  // ============================================================================

  "tail call in if body" in {
    val result = run("""
      |to mytest :n
      |  if :n > 0 [output mytest :n - 1]
      |  output 42
      |end
      |
      |print mytest 5
    """.stripMargin)
    result shouldBe "42"
  }

  "tail call in ifelse branches" in {
    val result = run("""
      |to collatz :n :steps
      |  if :n = 1 [output :steps]
      |  ifelse (remainder :n 2) = 0 [
      |    output collatz :n / 2 :steps + 1
      |  ] [
      |    output collatz :n * 3 + 1 :steps + 1
      |  ]
      |end
      |
      |print collatz 27 0
    """.stripMargin)
    // Collatz sequence for 27 takes 111 steps
    result shouldBe "111"
  }

  // ============================================================================
  // Non-Tail Recursive (should still work, just uses stack)
  // ============================================================================

  "non-tail recursion still works" in {
    val result = run("""
      |to factorial :n
      |  if :n = 0 [output 1]
      |  output :n * factorial :n - 1
      |end
      |
      |print factorial 10
    """.stripMargin)
    result shouldBe "3628800"
  }

  // Tree recursion - now works with full CPS
  "tree recursion" in {
    val result = run("""
      |to fib :n
      |  if :n <= 1 [output :n]
      |  output (fib :n - 1) + (fib :n - 2)
      |end
      |
      |print fib 20
    """.stripMargin)
    result shouldBe "6765"
  }

  // ============================================================================
  // Edge Cases
  // ============================================================================

  "output with expression" in {
    val result = run("""
      |to double :n
      |  output :n * 2
      |end
      |
      |print double 21
    """.stripMargin)
    result shouldBe "42"
  }

  "stop in procedure" in {
    val result = run("""
      |to mytest :n
      |  if :n = 0 [stop]
      |  print :n
      |  mytest :n - 1
      |end
      |
      |mytest 3
    """.stripMargin)
    result shouldBe "3\n2\n1"
  }

  // Nested procedure calls - now works with full CPS
  "nested procedure calls" in {
    val result = run("""
      |to outer :n
      |  if :n = 0 [output 0]
      |  output inner :n
      |end
      |
      |to inner :n
      |  output (outer :n - 1) + 1
      |end
      |
      |print outer 5
    """.stripMargin)
    result shouldBe "5"
  }

  // ============================================================================
  // Practical Examples
  // ============================================================================

  "gcd (Euclidean algorithm)" in {
    val result = run("""
      |to gcd :a :b
      |  if :b = 0 [output :a]
      |  output gcd :b remainder :a :b
      |end
      |
      |print gcd 48 18
    """.stripMargin)
    result shouldBe "6"
  }

  // ============================================================================
  // State Machine / Trampoline Pattern
  // ============================================================================

  "state machine with tail calls" in {
    val result = run("""
      |to statea :n
      |  if :n = 0 [output "a]
      |  output stateb :n - 1
      |end
      |
      |to stateb :n
      |  if :n = 0 [output "b]
      |  output statec :n - 1
      |end
      |
      |to statec :n
      |  if :n = 0 [output "c]
      |  output statea :n - 1
      |end
      |
      |print statea 10
    """.stripMargin)
    // 10 -> b, 9 -> c, 8 -> a, 7 -> b, 6 -> c, 5 -> a, 4 -> b, 3 -> c, 2 -> a, 1 -> b, 0 -> b
    result shouldBe "b"
  }

  // ============================================================================
  // List Processing with TCO
  // ============================================================================

  "list length with accumulator" in {
    val result = run("""
      |to length :lst
      |  output lengthhelper :lst 0
      |end
      |
      |to lengthhelper :lst :acc
      |  if emptyp :lst [output :acc]
      |  output lengthhelper butfirst :lst :acc + 1
      |end
      |
      |print length [a b c d e]
    """.stripMargin)
    result shouldBe "5"
  }

  "reverse list with accumulator" in {
    val result = run("""
      |to myreverse :lst
      |  output reversehelper :lst []
      |end
      |
      |to reversehelper :lst :acc
      |  if emptyp :lst [output :acc]
      |  output reversehelper butfirst :lst fput first :lst :acc
      |end
      |
      |print myreverse [1 2 3 4 5]
    """.stripMargin)
    result shouldBe "5 4 3 2 1"
  }
