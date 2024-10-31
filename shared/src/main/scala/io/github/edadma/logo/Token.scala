package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

sealed abstract class Token:
  var r: CharReader = null

  def pos(r: CharReader): LogoValue =
    this.r = r
    this

def tokenize(r: CharReader): Seq[Token] =
  r.consumeUpToDelim()
