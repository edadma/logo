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
              val res  = func(buf.toSeq)

              (res.pos(tok.r), rest)
        end if

  def display(v: LogoValue): String =
    v match
      case LogoNumber(n) => n
      case LogoWord(s)   => s
      case LogoNull()    => "null"
      case LogoList(l)   => l map display mkString " "
