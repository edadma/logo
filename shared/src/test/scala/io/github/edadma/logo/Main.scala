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
      |print sum 6 ( - ( 3 + 4 ) * 5 )
    """.trim.stripMargin
  val l = new Logo { override def event(): Unit = () }

  println(l.interp(s))
//  pprintln(tokenize(CharReader.fromString(s)))
//  pprintln(transform(tokenize(CharReader.fromString(s))))
