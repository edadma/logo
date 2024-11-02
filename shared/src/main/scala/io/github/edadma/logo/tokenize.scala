package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

def tokenize(r: CharReader): Seq[LogoValue] =
  val buf = new ListBuffer[LogoValue]

  @tailrec
  def tokenize(r: CharReader): Unit =
    val r2 = r.skipWhitespace

    if r2.eoi then buf += EOIToken().pos(r2)
    else
      val (s, r3) = r2.consume(r => r.ch.isWhitespace || r.ch == '[' || r.ch == ']')

      if s.nonEmpty then buf += LogoWord(s).pos(r2)

      r3.ch match
        case '[' =>
          buf += LogoWord("[").pos(r3)
          tokenize(r3.next)
        case ']' =>
          buf += LogoWord("]").pos(r3)
          tokenize(r3.next)
        case _ => tokenize(r3)

  tokenize(r)
  buf.toSeq
