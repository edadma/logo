package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

import scala.math.Pi

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

def number(v: LogoValue): Double =
  v match
    case LogoNumber(_, d) => d
    case _                => v.r.error("expected a number")

def list(v: LogoValue): Seq[LogoValue] =
  v match
    case LogoList(_, l) => l
    case _              => v.r.error("expected a list")

def normalizeAngle(angle: Double): Double =
  angle % (2 * Pi) match
    case h if h < 0 => h + 2 * Pi
    case h          => h

def radians(degrees: Double) = degrees * Pi / 180
