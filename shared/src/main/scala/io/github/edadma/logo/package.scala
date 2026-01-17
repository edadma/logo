package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader
import io.github.edadma.dal.QuaternionDAL

import scala.math.Pi

def logoNumber(n: Number): LogoNumber = LogoNumber(n)

def logoNumber(n: Int): LogoNumber = LogoNumber(n)

def logoNumber(n: Double): LogoNumber = LogoNumber(n)

def logoNumber(s: String, r: CharReader): LogoNumber =
  parseLogoNumber(s) match
    case Some(value) =>
      (r match
        case null => LogoNumber(value)
        case _    => LogoNumber(value).pos(r)
      ).asInstanceOf[LogoNumber]
    case None => problem(r, s"illegal number '$s'")

def parseLogoNumber(s: String): Option[Number] =
  if s.contains('.') then
    s.toDoubleOption.map(d => d: Number)
  else
    s.toIntOption match
      case Some(i) => Some(i: Number)
      case None    => s.toLongOption match
        case Some(l) => Some(l: Number)
        case None    => try Some(BigInt(s)) catch case _: NumberFormatException => None

def problem(pos: CharReader, error: String): Nothing =
  if (pos eq null)
    sys.error(error)
  else
    pos.error(error)

def number(v: LogoValue): Number =
  v match
    case LogoNumber(n) => n
    case w @ LogoWord(s) =>
      parseLogoNumber(s) match
        case Some(value) => value
        case None        => problem(w.r, s"expected a number: '$s'")
    case _ => v.r.error("expected a number")

def boolean(v: LogoValue): Boolean =
  v match
    case LogoBoolean(b)    => b
    case LogoWord("true")  => true
    case LogoWord("false") => false
    case _                 => v.r.error("expected a boolean")

def list(v: LogoValue): Seq[LogoValue] =
  v match
    case LogoList(_, l) => l
    case _              => v.r.error("expected a list")

def normalizeAngle(angle: Double): Double =
  angle % (2 * Pi) match
    case h if h < 0 => h + 2 * Pi
    case h          => h
