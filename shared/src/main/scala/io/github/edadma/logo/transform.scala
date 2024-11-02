package io.github.edadma.logo

def transform(toks: Seq[LogoValue]): Seq[LogoValue] =
  toks match
    case Nil      => Nil
    case hd :: tl => ???
