package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

sealed abstract class LogoValue:
  var r: CharReader = null

  def pos(r: CharReader): LogoValue =
    this.r = r
    this

case class LogoNumber(n: String, d: Double) extends LogoValue
case class LogoWord(s: String)              extends LogoValue
case class LogoNull()                       extends LogoValue
case class LogoList(list: Seq[LogoValue])   extends LogoValue
