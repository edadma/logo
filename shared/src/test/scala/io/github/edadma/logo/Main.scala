package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader
import pprint.pprintln

@main def run(): Unit =
  val s = "print reverse [apples and pears]"

  pprintln(tokenize(CharReader.fromString(s)))
