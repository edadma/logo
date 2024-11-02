package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec
import pprint.pprintln

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

class Logo:
//  def penForward(d: Double): Unit

  def interp(input: String): LogoValue = interp(CharReader.fromString(input))

  def interp(r: CharReader): LogoValue =
    @tailrec
    def interp(toks: Seq[LogoValue]): LogoValue =
      val (value, rest) = eval(toks)

      if rest.head.isInstanceOf[EOIToken] then value
      else interp(rest)

    val tokens = transform(tokenize(r))

    interp(tokens)

  case class Procedure(name: String, args: Int, func: PartialFunction[(Logo, Seq[LogoValue]), Any])

  private val procedures =
    List(
      Procedure(
        "print",
        1,
        {
          case (_, Seq(arg)) => println(arg)
        },
      ),
      Procedure(
        "sum",
        2,
        {
          case (_, Seq(left, right)) => number(left) + number(right)
        },
      ),
    ) map (p => p.name -> p) toMap

  private def number(v: LogoValue): Double =
    v match
      case LogoNumber(_, d) => d
      case _                => v.r.error("expected a number")

  def eval(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
    toks match
      case (v: (LogoNumber | LogoList | LogoNull)) :: tail => (v, tail)
      case (tok @ LogoWord(s)) :: tail =>
        if s.head == '"' then (LogoWord(s.tail).pos(tok.r.next), tail)
        else if s.head.isDigit then (logoNumber(s, tok.r), tail)
        else
          procedures get s match
            case None => tok.r.error(s"unknown procedure '$s'")
            case Some(Procedure(name, args, func)) =>
              val buf = new ListBuffer[LogoValue]

              @tailrec
              def evalarg(count: Int, toks: Seq[LogoValue]): Seq[LogoValue] =
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
                  case d: Double    => logoNumber(d)
                  case ()           => LogoNull()

              (res.pos(tok.r), rest)
        end if
