package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed abstract class Token:
  var r: CharReader = null

  def pos(r: CharReader): Token =
    this.r = r
    this

case class WordToken(s: String) extends Token
case class LeftBracketToken()   extends Token
case class RightBracketToken()  extends Token

def tokenize(r: CharReader): Seq[Token] =
  val buf = new ListBuffer[Token]

  @tailrec
  def tokenize(r: CharReader): Unit =
    val r2 = r.skipWhitespace

    if r2.more then
      val (s, r3) = r2.consume(r => r.ch.isWhitespace || r.ch == '[' || r.ch == ']')

      if s.nonEmpty then buf += WordToken(s).pos(r2)

      r3.ch match
        case '[' =>
          buf += LeftBracketToken().pos(r3)
          tokenize(r3.next)
        case ']' =>
          buf += RightBracketToken().pos(r3)
          tokenize(r3.next)
        case _ => tokenize(r3)

  tokenize(r)
  buf.toSeq
