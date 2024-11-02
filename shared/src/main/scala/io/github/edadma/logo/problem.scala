package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

def problem(pos: CharReader, error: String): Nothing =
  if (pos eq null)
    sys.error(error)
  else
    pos.error(error)
