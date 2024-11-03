package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec
import pprint.pprintln

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.math.{Pi, cos, sin}

abstract class Logo:
  def event(): Unit

  private[logo] var x: Double       = 0
  private[logo] var y: Double       = 0
  private[logo] var heading: Double = Pi / 2
  private[logo] var color: String   = "black"
  private[logo] var pen: Boolean    = true
  private[logo] var show: Boolean   = true
  private[logo] val draws           = new ListBuffer[Draw]
  private[logo] val vars            = new mutable.HashMap[String, LogoValue]

  event()

  def drawing: Seq[Draw]                       = draws.toSeq
  def turtle: Option[(Double, Double, Double)] = Option.when(show)(x, y, heading)

  def computeEndpoint(distance: Double): (Double, Double) = (x + distance * cos(heading), y + distance * sin(heading))
  def computeTurn(turn: Double): Double                   = normalizeAngle(heading - radians(turn))
  def computeHeading(heading: Double): Double             = normalizeAngle(Pi / 2 - radians(heading))

  def reset(): Unit =
    draws.clear()
    x = 0
    y = 0
    heading = Pi / 2
    color = "black"

  def interp(input: String): LogoValue = interp(CharReader.fromString(input))

  def interp(r: CharReader): LogoValue =

    val tokens = transform(tokenize(r))

    interp(tokens)

  @tailrec
  final def interp(toks: Seq[LogoValue]): LogoValue =
    val (value, rest) = eval(toks)

    if rest.head.isInstanceOf[EOIToken] then value
    else interp(rest)

  def lookup(proc: String): Option[Procedure | LogoValue] =
    val lower = proc.toLowerCase

    builtin get lower match
      case None =>
        synonyms get lower match
          case None => vars get lower
          case s    => s
      case p => p

  def eval(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
    toks match
      case List(EOIToken())                                => (LogoNull(), Seq(EOIToken()))
      case (v: (LogoNumber | LogoList | LogoNull)) :: tail => (v, tail)
      case (tok @ LogoWord("true" | "false")) :: tail      => (LogoBoolean(tok.toString == "true").pos(tok.r), tail)
      case (tok @ LogoWord("null")) :: tail                => (LogoNull().pos(tok.r), tail)
      case (tok @ LogoWord(s)) :: tail =>
        if s.head == '"' then (LogoWord(s.tail).pos(tok.r), tail)
        else if s.head.isDigit || (s.head == '-' && s != "-") then (logoNumber(s, tok.r), tail)
        else
          lookup(s) match
            case None => tok.r.error(s"unknown procedure or variable '$s'")
            case Some(Procedure(name, args, func)) =>
              val buf = new ListBuffer[LogoValue]

              @tailrec
              def evalarg(count: Int, toks: Seq[LogoValue]): Seq[LogoValue] =
                if count == 0 then toks
                else if toks.head.isInstanceOf[EOIToken] then
                  toks.head.r.error(s"unexpected end of input while evaluating argument(s) for '$name'")
                else
                  val (arg, rest) = eval(toks)

                  buf += arg
                  evalarg(count - 1, rest)

              val rest = evalarg(args, tail)
              val res =
                func(this, buf.toSeq) match
                  case v: LogoValue => v
                  case d: Double    => logoNumber(d)
                  case b: Boolean   => LogoBoolean(b)
                  case ()           => LogoNull()

              (res.pos(tok.r), rest)
            case Some(v: LogoValue) => (v, tail)
        end if
