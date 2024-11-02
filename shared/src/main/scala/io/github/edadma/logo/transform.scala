package io.github.edadma.logo

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

def transform(toks: Seq[LogoValue], list: Boolean = false): Seq[LogoValue] =
  val buf = new ListBuffer[LogoValue]

  @tailrec
  def transform(toks: Seq[LogoValue]): Seq[LogoValue] =
    toks match
      case Nil                 => Nil
      case LogoWord("[") :: tl => Nil
      case (w @ LogoWord(n)) :: tl if n.head.isDigit =>
        n.toDoubleOption match
          case Some(value) =>
            buf += LogoNumber(n, value).pos(w.r)
            transform(tl)
          case None => w.r.error(s"illegal number '$n'")
      case (w @ LogoWord(_)) :: tl =>
        buf += w
        transform(tl)
      case (eoi @ EOIToken()) :: _ =>
        buf += eoi
        Nil

  transform(toks)
  buf.toSeq
