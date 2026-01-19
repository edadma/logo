package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader

sealed abstract class LogoValue:
  var r: CharReader = null

  def pos(r: CharReader): LogoValue =
    this.r = r
    this

case class LogoNumber(n: Number) extends LogoValue:
  override def toString: String =
    n match
      case i: java.lang.Integer => i.toString
      case l: java.lang.Long    => l.toString
      case bi: BigInt           => bi.toString
      case d: java.lang.Double =>
        if d == d.toLong then d.toLong.toString
        else d.toString
      case _ => n.toString

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

// Markers for pending operations - eval returns these, interp handles them with CPS
case class PendingCallMarker(proc: UserProcedure, args: Seq[LogoValue]) extends LogoValue:
  override def toString: String = s"<pending:${proc.name}>"
case class PendingIf(cond: Boolean, body: Seq[LogoValue]) extends LogoValue:
  override def toString: String = "<pending:if>"
case class PendingIfElse(cond: Boolean, yesBody: Seq[LogoValue], noBody: Seq[LogoValue]) extends LogoValue:
  override def toString: String = "<pending:ifelse>"
case class PendingRepeat(times: Int, body: Seq[LogoValue]) extends LogoValue:
  override def toString: String = "<pending:repeat>"
case class PendingRun(code: String) extends LogoValue:
  override def toString: String = "<pending:run>"
case class PendingOutput(value: LogoValue) extends LogoValue:
  override def toString: String = "<pending:output>"
