package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader
import pprint.pprintln

@main def run(): Unit =
//  val s = "print reverse [apples and pears]"
//
//  pprintln(tokenize(CharReader.fromString(s)))

  val s =
    """
      |print sum 3 4
      |print "asdf
      |print [one two]
      |""".trim.stripMargin
  val l = new Logo

  println(l.interp(s))
