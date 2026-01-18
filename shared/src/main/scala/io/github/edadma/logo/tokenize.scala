package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

private val operatorChars = Set('+', '*', '/', '\\', '^', '=', '<', '>')
private def isOperatorChar(c: Char): Boolean = operatorChars.contains(c)
private def isTokenBoundary(r: CharReader, inList: Boolean): Boolean =
  r.ch.isWhitespace || r.ch == '[' || r.ch == ']' || r.ch == '(' || r.ch == ')' || (!inList && isOperatorChar(r.ch))

def tokenize(r: CharReader): Seq[LogoValue] =
  val buf = new ListBuffer[LogoValue]

  // afterWhitespace: true if we just skipped whitespace (value position expected)
  @tailrec
  def tokenize(r: CharReader, listDepth: Int, afterWhitespace: Boolean): Unit =
    val r2 = r.skipWhitespace
    val skippedWhitespace = r2 != r || afterWhitespace
    val inList = listDepth > 0

    if r2.eoi then buf += EOIToken().pos(r2)
    else
      r2.ch match
        case ';' =>
          // Comment - skip to end of line
          val (_, r3) = r2.consume(r => r.ch == '\n' || r.ch == '\r' || r.eoi)
          tokenize(r3, listDepth, true)
        case '[' =>
          buf += LogoWord("[").pos(r2)
          tokenize(r2.next, listDepth + 1, false)
        case ']' =>
          buf += LogoWord("]").pos(r2)
          tokenize(r2.next, (listDepth - 1) max 0, false)
        case '(' if !inList =>
          buf += LogoWord("(").pos(r2)
          tokenize(r2.next, listDepth, true)
        case ')' if !inList =>
          buf += LogoWord(")").pos(r2)
          tokenize(r2.next, listDepth, false)
        case '<' if !inList =>
          // Handle <, <>, <=
          val r3 = r2.next
          if !r3.eoi && (r3.ch == '>' || r3.ch == '=') then
            buf += LogoWord(s"<${r3.ch}").pos(r2)
            tokenize(r3.next, listDepth, false)
          else
            buf += LogoWord("<").pos(r2)
            tokenize(r3, listDepth, false)
        case '>' if !inList =>
          // Handle >, >=
          val r3 = r2.next
          if !r3.eoi && r3.ch == '=' then
            buf += LogoWord(">=").pos(r2)
            tokenize(r3.next, listDepth, false)
          else
            buf += LogoWord(">").pos(r2)
            tokenize(r3, listDepth, false)
        case '/' if !inList =>
          // Handle / and //
          val r3 = r2.next
          if !r3.eoi && r3.ch == '/' then
            buf += LogoWord("//").pos(r2)
            tokenize(r3.next, listDepth, false)
          else
            buf += LogoWord("/").pos(r2)
            tokenize(r3, listDepth, false)
        case c if !inList && isOperatorChar(c) =>
          // Single-char operators: + * ^ =
          buf += LogoWord(c.toString).pos(r2)
          tokenize(r2.next, listDepth, false)
        case '-' if !inList =>
          // Minus is special: could be operator or negative number
          // Only treat as negative number if after whitespace (value position)
          val r3 = r2.next
          if skippedWhitespace && !r3.eoi && r3.ch.isDigit then
            // Negative number - consume the whole number
            val (s, r4) = r3.consume(r => isTokenBoundary(r, inList))
            buf += LogoWord(s"-$s").pos(r2)
            tokenize(r4, listDepth, false)
          else
            // Minus operator
            buf += LogoWord("-").pos(r2)
            tokenize(r3, listDepth, false)
        case _ =>
          // Regular word/number - inside lists, don't split on operators
          val (s, r3) = r2.consume(r => isTokenBoundary(r, inList) || (!inList && r.ch == '-'))
          if s.nonEmpty then buf += LogoWord(s).pos(r2)
          tokenize(r3, listDepth, false)

  tokenize(r, 0, true)
  buf.toSeq
