package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec
import pprint.pprintln

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

class Logo:
//  def penForward(d: Double): Unit

  def interp(input: String): String = interp(CharReader.fromString(input))

  def interp(r: CharReader): String =
    @tailrec
    def interp(toks: Seq[Token]): String =
      val (value, rest) = eval(toks)

      if rest.head.isInstanceOf[EOIToken] then display(value)
      else interp(toks)

    val tokens = tokenize(r)

    interp(tokens)

  case class Procedure(name: String, args: Int, func: PartialFunction[Seq[LogoValue], Any])

  private val procedures =
    List(
      Procedure(
        "print",
        1,
        {
          case Seq(arg) => println(display(arg))
        },
      ),
      Procedure(
        "sum",
        2,
        {
          case Seq(left, right) => number(left) + number(right)
        },
      ),
    ) map (p => p.name -> p) toMap

  private def numeric(n: Double): LogoNumber =
    val s =
      if n.isWhole then n.toInt.toString
      else n.toString

    LogoNumber(s, n)

  private def number(v: LogoValue): Double =
    v match
      case LogoNumber(_, d) => d
      case _                => v.r.error("expected a number")

  def eval(toks: Seq[Token]): (LogoValue, Seq[Token]) =
    toks match
      case (tok @ WordToken(s)) :: tail =>
        if s.head == '"' then (LogoWord(s.tail).pos(tok.r.next), tail)
        else if s.head.isDigit then
          s.toDoubleOption match
            case Some(value) => (LogoNumber(s, value).pos(tok.r), tail)
            case None        => tok.r.error(s"illegal number '$s'")
        else
          procedures get s match
            case None => tok.r.error(s"unknown procedure '$s'")
            case Some(Procedure(name, args, func)) =>
              val buf = new ListBuffer[LogoValue]

              @tailrec
              def evalarg(count: Int, toks: Seq[Token]): Seq[Token] =
                if count == 0 then toks
                else if toks.head.isInstanceOf[EOIToken] then
                  toks.head.r.error(s"unexpected end of input which evaluating argument(s) for '$name'")
                else
                  val (arg, rest) = eval(toks)

                  buf += arg
                  evalarg(count - 1, rest)

              val rest = evalarg(args, tail)
              val res =
                func(buf.toSeq) match
                  case v: LogoValue => v
                  case d: Double    => numeric(d)
                  case ()           => LogoNull()

              (res.pos(tok.r), rest)
        end if

  def display(v: LogoValue): String =
    v match
      case LogoNumber(n, _) => n
      case LogoWord(s)      => s
      case LogoNull()       => "null"
      case LogoList(l)      => l map display mkString " "
