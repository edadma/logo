package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader
import pprint.pprintln

@main def run(): Unit =
//  val s = "print reverse [apples and pears]"
////  val s = "print 123"
//
//  pprintln(transform(tokenize(CharReader.fromString(s))))

  val s =
    """
      |print + 3 4
      |print "asdf
      |print [one two]
      |""".trim.stripMargin
  val l = new Logo { override def event(): Unit = () }

  println(l.interp(s))
