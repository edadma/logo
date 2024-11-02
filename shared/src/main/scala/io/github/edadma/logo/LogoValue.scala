package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

sealed abstract class LogoValue:
  var r: CharReader = null

  def pos(r: CharReader): LogoValue =
    this.r = r
    this

case class LogoNumber(text: String, d: Double) extends LogoValue
case class LogoWord(text: String)              extends LogoValue
case class LogoNull()                          extends LogoValue { override def toString: String = "null"            }
case class LogoList(list: Seq[LogoValue])      extends LogoValue { override def toString: String = list mkString " " }
case class EOIToken()                          extends LogoValue { override def toString: String = ""                }
