package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

abstract class Logo:
  def penForward(d: Double): Unit

  def interp(input: String): Unit = interp(CharReader.fromString(input))

  def interp(r: CharReader): Unit =
    tokenize(r)
