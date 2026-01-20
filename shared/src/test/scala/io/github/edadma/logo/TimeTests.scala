package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TimeTests extends AnyFreeSpec with Matchers with Test:
  // time tests
  "time returns a list of 3 elements" in {
    eval("count time") shouldBe "3"
  }

  "time first element is hours (0-23)" in {
    val hours = eval("first time").toInt
    hours should be >= 0
    hours should be <= 23
  }

  "time second element is minutes (0-59)" in {
    val minutes = eval("first butfirst time").toInt
    minutes should be >= 0
    minutes should be <= 59
  }

  "time third element is seconds (0-59)" in {
    val seconds = eval("last time").toInt
    seconds should be >= 0
    seconds should be <= 59
  }

  // date tests
  "date returns a list of 3 elements" in {
    eval("count date") shouldBe "3"
  }

  "date first element is year" in {
    val year = eval("first date").toInt
    year should be >= 2020
    year should be <= 2100
  }

  "date second element is month (1-12)" in {
    val month = eval("first butfirst date").toInt
    month should be >= 1
    month should be <= 12
  }

  "date third element is day (1-31)" in {
    val day = eval("last date").toInt
    day should be >= 1
    day should be <= 31
  }

  // timemilli tests
  "timemilli returns a number" in {
    eval("numberp timemilli") shouldBe "true"
  }

  "timemilli returns increasing values" in {
    val t1 = eval("timemilli").toLong
    val t2 = eval("timemilli").toLong
    t2 should be >= t1
  }

  "timemilli useful for benchmarking" in {
    // Verify timemilli returns a large positive number (milliseconds since epoch)
    val t = eval("timemilli").toLong
    // Should be after year 2020 (1577836800000 ms)
    t should be > 1577836800000L
  }

  // forever tests - stop exits the current procedure
  "forever with immediate stop" in {
    run(
      """
        |to test
        |  make "count 0
        |  forever [
        |    make "count :count + 1
        |    if :count = 5 [stop]
        |  ]
        |  print "after
        |end
        |test
        |print :count
        |""".stripMargin
    ) shouldBe "5"
  }

  "forever accumulates values" in {
    run(
      """
        |to test
        |  make "sum 0
        |  make "i 1
        |  forever [
        |    make "sum :sum + :i
        |    make "i :i + 1
        |    if :i > 10 [stop]
        |  ]
        |end
        |test
        |print :sum
        |""".stripMargin
    ) shouldBe "55"
  }

  "forever exits on output" in {
    run(
      """
        |to countdown :n
        |  forever [
        |    print :n
        |    make "n :n - 1
        |    if :n < 1 [output "done]
        |  ]
        |end
        |print countdown 3
        |""".stripMargin
    ) shouldBe "3\n2\n1\ndone"
  }
