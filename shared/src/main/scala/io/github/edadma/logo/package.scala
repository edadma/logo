package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

def logoNumber(n: Double): LogoNumber =
  val s =
    if n.isWhole then n.toInt.toString
    else n.toString

  LogoNumber(s, n)

def logoNumber(s: String, r: CharReader): LogoNumber =
  s.toDoubleOption match
    case Some(value) =>
      (r match
        case null => LogoNumber(s, value)
        case _    => LogoNumber(s, value).pos(r)
      ).asInstanceOf[LogoNumber]
    case None => problem(r, s"illegal number '$s'")

def problem(pos: CharReader, error: String): Nothing =
  if (pos eq null)
    sys.error(error)
  else
    pos.error(error)
