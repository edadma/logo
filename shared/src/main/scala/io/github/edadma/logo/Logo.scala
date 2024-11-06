package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec
import pprint.pprintln

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.math.{Pi, cos, sin, toRadians}

abstract class Logo:
  def event(): Unit

  private[logo] var x: Double              = 0
  private[logo] var y: Double              = 0
  private[logo] var heading: Double        = Pi / 2
  private[logo] var color: (Int, Int, Int) = colorMap("black")
  private[logo] var pen: Boolean           = true
  private[logo] var width: Double          = 1
  private[logo] var show: Boolean          = true
  private[logo] val draws                  = new ListBuffer[Draw]
  private[logo] val vars                   = new mutable.HashMap[String, LogoValue]

  event()

  def drawing: Seq[Draw]                       = draws.toSeq
  def turtle: Option[(Double, Double, Double)] = Option.when(show)(x, y, heading)

  def computeEndpoint(distance: Double): (Double, Double) = (x + distance * cos(heading), y + distance * sin(heading))
  def computeTurn(turn: Double): Double                   = normalizeAngle(heading - toRadians(turn))
  def computeHeading(heading: Double): Double             = normalizeAngle(Pi / 2 - toRadians(heading))

  def home(): Unit =
    x = 0
    y = 0
    heading = Pi / 2

  def clearscreen(): Unit =
    draws.clear()
    home()
    color = colorMap("black")
    pen = true
    width = 1

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

  def variable(name: String): Option[LogoValue] = (vars get name.toLowerCase) map (_.clone.asInstanceOf[LogoValue])

  private def evalArguments(name: String, count: Int, toks: Seq[LogoValue]): (Seq[LogoValue], Seq[LogoValue]) =
    val buf = new ListBuffer[LogoValue]

    @tailrec
    def evalArguments(count: Int, toks: Seq[LogoValue]): Seq[LogoValue] =
      if count == 0 then toks
      else if toks.head.isInstanceOf[EOIToken] then
        toks.head.r.error(s"unexpected end of input while evaluating argument(s) for '$name'")
      else
        val (arg, rest) = eval(toks)

        buf += arg
        evalArguments(count - 1, rest)

    val rest = evalArguments(count, toks)

    (buf.toSeq, rest)

  def nevalArguments(name: String, count: Int, toks: Seq[LogoValue]): (Seq[Double], Seq[LogoValue]) =
    val (args, rest) = evalArguments(name, count, toks)

    (args map number, rest)

  def eval(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) = evalAdditive(toks)

  def evalAdditive(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
    @tailrec
    def evalAdditiveTail(left: LogoValue, toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
      toks match
        case (op @ LogoWord("+" | "-")) :: tail =>
          val (right, remaining) = evalMultiplicative(tail)
          val l                  = number(left)
          val r                  = number(right)
          val res =
            op.toString match
              case "+" => l + r
              case "-" => l - r

          evalAdditiveTail(logoNumber(res), remaining)
        case _ => (left, toks)

    val (left, rest) = evalMultiplicative(toks)

    evalAdditiveTail(left, rest)

  def evalMultiplicative(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
    @tailrec
    def evalMultiplicativeTail(left: LogoValue, toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
      toks match
        case (op @ LogoWord("*" | "/" | "%")) :: tail =>
          val (right, remaining) = evalPrimary(tail)
          val l                  = number(left)
          val r                  = number(right)
          val res =
            op.toString match
              case "*" => l * r
              case "/" => l / r
              case "%" => l % r

          evalMultiplicativeTail(logoNumber(res), remaining)
        case _ => (left, toks)

    val (left, rest) = evalPrimary(toks)

    evalMultiplicativeTail(left, rest)

  def evalPrimary(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
    toks match
      case List(EOIToken())                                => (LogoNull(), Seq(EOIToken()))
      case (v: (LogoNumber | LogoList | LogoNull)) :: tail => (v, tail)
      case (tok @ LogoWord("true" | "false")) :: tail      => (LogoBoolean(tok.toString == "true").pos(tok.r), tail)
      case (tok @ LogoWord("null")) :: tail                => (LogoNull().pos(tok.r), tail)
      case (tok @ LogoWord("-")) :: tail =>
        val (v, remaining) = evalPrimary(tail)

        (logoNumber(-number(v)).pos(tok.r), remaining)
      case LogoWord("(") :: tail =>
        val (v, afterExpr) = eval(tail)

        afterExpr match
          case LogoWord(")") :: remaining => (v, remaining)
          case (tok: LogoValue) :: _      => tok.r.error("Expected ')'")
      case (tok @ LogoWord(s)) :: tail if s.head == '"' => (LogoWord(s.tail).pos(tok.r), tail)
      case (tok @ LogoWord(s)) :: tail if s.head == ':' =>
        variable(s.tail) match
          case Some(v) => (v.pos(tok.r), tail)
          case None    => tok.r.next.error(s"unknown variable '${s.tail}'")
      case (tok @ LogoWord(s)) :: tail if s.head.isDigit || s.head == '-' => (logoNumber(s, tok.r), tail)
      case (tok @ LogoWord(s)) :: tail =>
        lookup(s) match
          case None                            => tok.r.error(s"unknown procedure, variable, or constant '$s'")
          case Some(BuiltinFunction0(_, func)) => (logoNumber(func()).pos(tok.r), tail)
          case Some(BuiltinFunction1(name, func)) =>
            val (Seq(a), rest) = nevalArguments(name, 1, tail)

            (logoNumber(func(a)).pos(tok.r), rest)
          case Some(BuiltinFunction2(name, func)) =>
            val (Seq(a, b), rest) = nevalArguments(name, 2, tail)

            (logoNumber(func(a, b)).pos(tok.r), rest)
          case Some(BuiltinProcedure(name, args, func)) =>
            val (vals, rest) = evalArguments(name, args, tail)
            val res =
              func(this, vals) match
                case v: LogoValue => v
                case d: Double    => logoNumber(d)
                case b: Boolean   => LogoBoolean(b)
                case ()           => LogoNull()

            (res.pos(tok.r), rest)
          case Some(v: LogoValue) => (v, tail)
          case Some(p: Procedure) => problem(tok.r, s"procedure of unknown type: '${p.name}'")
