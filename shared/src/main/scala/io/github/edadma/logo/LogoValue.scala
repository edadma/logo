package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

sealed abstract class LogoValue:
  var r: CharReader = null

  def pos(r: CharReader): LogoValue =
    this.r = r
    this

case class LogoNumber(n: Double) extends LogoValue
case class LogoString(s: String) extends LogoValue
