package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec
import pprint.pprintln

import scala.language.postfixOps

class Logo:
//  def penForward(d: Double): Unit

  def interp(input: String): String = interp(CharReader.fromString(input))

  def interp(r: CharReader): String =
    val tokens = tokenize(r)

    pprintln(tokens)
    @tailrec
    def interp(toks: Seq[Token]): String =
      val (value, rest) = eval(toks)

      if rest.isEmpty then display(value)
      else interp(toks)

    interp(tokens)

  case class Procedure(name: String, args: Int, func: PartialFunction[Seq[LogoValue], LogoValue])

  private val procedures =
    List(
      Procedure(
        "print",
        1,
        {
          case Seq(arg) =>
            println(display(arg))
            LogoNull()
        },
      ),
    ) map (p => p.name -> p) toMap

  def eval(toks: Seq[Token]): (LogoValue, Seq[Token]) =
    toks match
      case (tok @ WordToken(s)) :: tail =>
        if s.head == '"' then (LogoWord(s.tail).pos(tok.r.next), tail)
        else if s.head.isDigit then (LogoNumber(s).pos(tok.r), tail)
        else
          procedures get s match
            case None => tok.r.error(s"unknown procedure '$s'")
            case Some(Procedure(name, args, func)) =>
              val (arg, rest) = eval(tail)
              val res         = func(Seq(arg))

              (res.pos(tok.r), rest)

  def display(v: LogoValue): String =
    v match
      case LogoNumber(n) => n
      case LogoWord(s)   => s
      case LogoNull()    => "null"
      case LogoList(l)   => l map display mkString " "
