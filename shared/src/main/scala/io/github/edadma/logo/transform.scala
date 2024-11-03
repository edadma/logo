package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

def transform(toks: Seq[LogoValue]): Seq[LogoValue] =
  val buf = new ListBuffer[LogoValue]

  @tailrec
  def transform(toks: Seq[LogoValue]): Seq[LogoValue] =
    toks match
      case Nil => Nil
      case (start @ LogoWord("[")) :: tl =>
        val (list, rest) = transformList(start.r, tl)

        buf += list
        transform(rest)
      case (w @ LogoWord(n)) :: tl if n.head.isDigit =>
        buf += logoNumber(n, w.r)
        transform(tl)
      case (w @ LogoWord(_)) :: tl =>
        buf += w
        transform(tl)
      case (eoi @ EOIToken()) :: _ =>
        buf += eoi
        Nil

  transform(toks)
  buf.toSeq

def transformList(
    start: CharReader,
    toks: Seq[LogoValue],
    buf: ListBuffer[LogoValue] = new ListBuffer,
): (LogoValue, Seq[LogoValue]) =
  toks match
    case Nil => sys.error("unexpected end of token list")
    case (startsub @ LogoWord("[")) :: tl =>
      val (list, rest) = transformList(startsub.r, tl)

      buf += list
      transformList(start, rest, buf)
    case (end @ LogoWord("]")) :: tl =>
      val list = buf.toSeq

      buf += EOIToken().pos(end.r)
      (LogoList(list, buf.toSeq).pos(start), tl)
    case (w @ LogoWord(n)) :: tl if n.head.isDigit || n.head == '-' =>
      n.toDoubleOption match
        case Some(value) => buf += LogoNumber(n, value).pos(w.r)
        case None        => buf += w
      transformList(start, tl, buf)
    case (w @ LogoWord(_)) :: tl =>
      buf += w
      transformList(start, tl, buf)
    case (eoi @ EOIToken()) :: _ => eoi.r.error("unclosed list")
