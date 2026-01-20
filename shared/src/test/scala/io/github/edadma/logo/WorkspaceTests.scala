package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class WorkspaceTests extends AnyFreeSpec with Matchers with Test:
  // namep tests
  "namep returns false for undefined variable" in {
    eval("namep \"x") shouldBe "false"
  }

  "namep returns true for defined variable" in {
    eval("make \"x 42 namep \"x") shouldBe "true"
  }

  "name? synonym works" in {
    eval("make \"y 10 name? \"y") shouldBe "true"
  }

  // definedp tests
  "definedp returns true for builtin" in {
    eval("definedp \"forward") shouldBe "true"
  }

  "definedp returns true for synonym" in {
    eval("definedp \"fd") shouldBe "true"
  }

  "definedp returns false for undefined" in {
    eval("definedp \"notaproc") shouldBe "false"
  }

  "definedp returns true for user procedure" in {
    eval(
      """
        |to square :x
        |  output :x * :x
        |end
        |definedp "square
        |""".stripMargin,
    ) shouldBe "true"
  }

  "defined? synonym works" in {
    eval("defined? \"print") shouldBe "true"
  }

  // primitivep tests
  "primitivep returns true for builtin" in {
    eval("primitivep \"print") shouldBe "true"
  }

  "primitivep returns true for synonym" in {
    eval("primitivep \"pr") shouldBe "true"
  }

  "primitivep returns false for user procedure" in {
    eval(
      """
        |to myproc
        |  print 1
        |end
        |primitivep "myproc
        |""".stripMargin,
    ) shouldBe "false"
  }

  "primitive? synonym works" in {
    eval("primitive? \"sum") shouldBe "true"
  }

  // procedurep tests
  "procedurep returns false for builtin" in {
    eval("procedurep \"print") shouldBe "false"
  }

  "procedurep returns true for user procedure" in {
    eval(
      """
        |to foo
        |  output 1
        |end
        |procedurep "foo
        |""".stripMargin,
    ) shouldBe "true"
  }

  "procedure? synonym works" in {
    eval("procedure? \"forward") shouldBe "false"
  }

  // procedures tests
  "procedures returns empty list initially" in {
    eval("procedures") shouldBe ""
  }

  "procedures returns user-defined procedure names" in {
    eval(
      """
        |to aaa
        |  output 1
        |end
        |to bbb
        |  output 2
        |end
        |procedures
        |""".stripMargin,
    ) shouldBe "aaa bbb"
  }

  // primitives tests
  "primitives returns non-empty list" in {
    eval("emptyp primitives") shouldBe "false"
  }

  "primitives contains forward" in {
    eval("memberp \"forward primitives") shouldBe "true"
  }

  "primitives contains synonyms" in {
    eval("memberp \"fd primitives") shouldBe "true"
  }

  // names tests
  "names returns empty when no variables" in {
    eval("names") shouldBe "[] []"
  }

  "names returns variable names" in {
    eval(
      """
        |make "a 1
        |make "b 2
        |names
        |""".stripMargin,
    ) shouldBe "[] [a b]"
  }
