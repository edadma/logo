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
      |make "a 123
      |print :a
    """.trim.stripMargin
  val l = new Logo { override def event(): Unit = () }

  println(l.interp(s))
//  pprintln(tokenize(CharReader.fromString(s)))
//  pprintln(transform(tokenize(CharReader.fromString(s))))
