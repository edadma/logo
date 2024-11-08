package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

sealed abstract class LogoValue:
  var r: CharReader     = null
  var readonly: Boolean = false

  def pos(r: CharReader): LogoValue =
    if !readonly then this.r = r
    this

  def seal: LogoValue =
    r = null
    readonly = true
    this

case class LogoNumber(override val toString: String, n: Double)          extends LogoValue
case class LogoProcedure(override val toString: String, proc: Procedure) extends LogoValue
case class LogoWord(override val toString: String)                       extends LogoValue
case class LogoNull()              extends LogoValue { override val toString: String = "null"                        }
case class LogoBoolean(b: Boolean) extends LogoValue { override def toString: String = if b then "true" else "false" }
case class LogoList(list: Seq[LogoValue], terminated: Seq[LogoValue]) extends LogoValue:
  override def toString: String = list map {
    case LogoList(sublist, _) => sublist.mkString("[", " ", "]")
    case v                    => v.toString
  } mkString " "
case class EOIToken() extends LogoValue { override def toString: String = "" }
