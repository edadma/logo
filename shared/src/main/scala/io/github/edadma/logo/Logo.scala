package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader
import io.github.edadma.dal.QuaternionDAL

import scala.annotation.tailrec
import pprint.pprintln

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.math.{Pi, cos, sin, toRadians}

// Exceptions for control flow in user procedures
case class OutputException(value: LogoValue) extends Exception
case class StopException()                   extends Exception

abstract class Logo:
  def event(): Unit

  private[logo] var x: Double              = 0
  private[logo] var y: Double              = 0
  private[logo] var heading: Double        = Pi / 2
  private[logo] var color: (Int, Int, Int) = colorMap("black")
  private[logo] var pen: Boolean           = true
  private[logo] var width: Double          = 1
  private[logo] var show: Boolean          = true
  private[logo] val draws                  = new ListBuffer[Draw]
  private[logo] val vars                   = new mutable.HashMap[String, LogoValue]
  private[logo] val procedures             = new mutable.HashMap[String, UserProcedure]

  event()

  def drawing: Seq[Draw]                       = draws.toSeq
  def turtle: Option[(Double, Double, Double)] = Option.when(show)(x, y, heading)

  def computeEndpoint(distance: Double): (Double, Double) = (x + distance * cos(heading), y + distance * sin(heading))
  def computeTurn(turn: Double): Double                   = normalizeAngle(heading - toRadians(turn))
  def computeHeading(heading: Double): Double             = normalizeAngle(Pi / 2 - toRadians(heading))

  def home(): Unit =
    x = 0
    y = 0
    heading = Pi / 2

  def clearscreen(): Unit =
    draws.clear()
    home()
    color = colorMap("black")
    pen = true
    width = 1

  def interp(input: String): LogoValue = interp(CharReader.fromString(input))

  def interp(r: CharReader): LogoValue =

    val tokens = transform(tokenize(r))

    interp(tokens)

  @tailrec
  final def interp(toks: Seq[LogoValue]): LogoValue =
    val (value, rest) = eval(toks)

    if rest.head.isInstanceOf[EOIToken] then value
    else interp(rest)

  def lookup(proc: String): Option[Procedure | LogoValue] =
    val lower = proc.toLowerCase

    builtin get lower match
      case None =>
        synonyms get lower match
          case None =>
            procedures get lower match
              case None => vars get lower
              case p    => p
          case s => s
      case p => p

  private def evalargs(name: String, count: Int, toks: Seq[LogoValue]): (Seq[LogoValue], Seq[LogoValue]) =
    val buf = new ListBuffer[LogoValue]

    @tailrec
    def evalargs(count: Int, toks: Seq[LogoValue]): Seq[LogoValue] =
      if count == 0 then toks
      else if toks.head.isInstanceOf[EOIToken] then
        toks.head.r.error(s"unexpected end of input while evaluating argument(s) for '$name'")
      else
        val (arg, rest) = eval(toks)

        buf += arg
        evalargs(count - 1, rest)

    val rest = evalargs(count, toks)

    (buf.toSeq, rest)

  def evalargsn(name: String, count: Int, toks: Seq[LogoValue]): (Seq[Number], Seq[LogoValue]) =
    val (args, rest) = evalargs(name, count, toks)

    (args map number, rest)

  private def evalArgsUntilParen(name: String, minArgs: Int, toks: Seq[LogoValue]): (Seq[LogoValue], Seq[LogoValue]) =
    val buf = new ListBuffer[LogoValue]

    @tailrec
    def loop(toks: Seq[LogoValue]): Seq[LogoValue] =
      toks match
        case LogoWord(")") :: rest => rest
        case (eoi: EOIToken) :: _  => eoi.r.error(s"expected closing parenthesis for variadic call to '$name'")
        case _ =>
          val (arg, rest) = eval(toks)
          buf += arg
          loop(rest)

    val rest = loop(toks)
    if buf.size < minArgs then
      problem(null, s"'$name' requires at least $minArgs argument(s), got ${buf.size}")
    (buf.toSeq, rest)

  def eval(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) = evalComparison(toks)

  private def evalComparison(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
    val (left, rest) = evalAdditive(toks)
    rest match
      case LogoWord(op @ ("=" | "<>" | "<" | ">" | "<=" | ">=")) :: tail =>
        val (right, rest2) = evalAdditive(tail)
        val result = op match
          case "="  => LogoBoolean(left == right)
          case "<>" => LogoBoolean(left != right)
          case "<"  => LogoBoolean(QuaternionDAL.relate("<", number(left), number(right)))
          case ">"  => LogoBoolean(QuaternionDAL.relate(">", number(left), number(right)))
          case "<=" => LogoBoolean(QuaternionDAL.relate("<=", number(left), number(right)))
          case ">=" => LogoBoolean(QuaternionDAL.relate(">=", number(left), number(right)))
        (result.pos(left.r), rest2)
      case _ => (left, rest)

  private def evalAdditive(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
    @tailrec
    def loop(left: LogoValue, toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
      toks match
        case LogoWord(op @ ("+" | "-")) :: tail =>
          val (right, rest) = evalMultiplicative(tail)
          val result = logoNumber(QuaternionDAL.compute(op, number(left), number(right)))
          loop(result, rest)
        case _ => (left, toks)

    val (left, rest) = evalMultiplicative(toks)
    loop(left, rest)

  private def evalMultiplicative(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
    @tailrec
    def loop(left: LogoValue, toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
      toks match
        case LogoWord("*") :: tail =>
          val (right, rest) = evalPower(tail)
          val result = logoNumber(QuaternionDAL.compute("*", number(left), number(right)))
          loop(result, rest)
        case LogoWord("/") :: tail =>
          // Exact arithmetic division
          val (right, rest) = evalPower(tail)
          val result = logoNumber(QuaternionDAL.compute("/", number(left), number(right)))
          loop(result, rest)
        case LogoWord("\\") :: tail =>
          // Float division - always returns Double
          val (right, rest) = evalPower(tail)
          val result = logoNumber(number(left).doubleValue / number(right).doubleValue)
          loop(result, rest)
        case LogoWord("//") :: tail =>
          // Floor division - returns integer
          val (right, rest) = evalPower(tail)
          val result = logoNumber(math.floor(number(left).doubleValue / number(right).doubleValue).toLong)
          loop(result, rest)
        case _ => (left, toks)

    val (left, rest) = evalPower(toks)
    loop(left, rest)

  // Power is right-associative: 2^3^2 = 2^(3^2) = 2^9 = 512
  private def evalPower(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
    val (left, rest) = evalPrimary(toks)
    rest match
      case LogoWord("^") :: tail =>
        val (right, rest2) = evalPower(tail) // right-associative: recurse instead of loop
        val result = logoNumber(QuaternionDAL.compute("^", number(left), number(right)))
        (result.pos(left.r), rest2)
      case _ => (left, rest)

  private def evalPrimary(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
    toks match
      case List(EOIToken())                                => (LogoNull(), Seq(EOIToken()))
      case (v: (LogoNumber | LogoList | LogoNull)) :: tail => (v, tail)
      case (tok @ LogoWord("true" | "false")) :: tail      => (LogoBoolean(tok.toString == "true").pos(tok.r), tail)
      case (tok @ LogoWord("null")) :: tail                => (LogoNull().pos(tok.r), tail)
      case (tok @ LogoWord("(")) :: tail =>
        // Check if this is a variadic procedure call or expression grouping
        tail match
          case (procTok @ LogoWord(procName)) :: rest if !procName.head.isDigit && procName.head != '"' && procName.head != ':' =>
            lookup(procName) match
              case Some(BuiltinVariadic(name, _, minArgs, func)) =>
                // Variadic procedure call - collect args until )
                val (args, rest2) = evalArgsUntilParen(name, minArgs, rest)
                val res = func(this, args) match
                  case v: LogoValue => v
                  case n: Number    => logoNumber(n)
                  case b: Boolean   => LogoBoolean(b)
                  case ()           => LogoNull()
                (res.pos(tok.r), rest2)
              case Some(up @ UserProcedure(name, reqParams, optParams, restParam, _))
                  if optParams.nonEmpty || restParam.isDefined =>
                // User-defined variadic procedure call - collect args until )
                val (args, rest2) = evalArgsUntilParen(name, reqParams.length, rest)
                val result = callUserProc(up, args)
                (result.pos(tok.r), rest2)
              case _ =>
                // Not a variadic procedure - treat as expression grouping
                val (value, rest2) = eval(tail)
                rest2 match
                  case LogoWord(")") :: rest3 => (value.pos(tok.r), rest3)
                  case _                      => tok.r.error("expected closing parenthesis")
          case _ =>
            // Expression grouping
            val (value, rest) = eval(tail)
            rest match
              case LogoWord(")") :: rest2 => (value.pos(tok.r), rest2)
              case _                      => tok.r.error("expected closing parenthesis")
      case (tok @ LogoWord("to")) :: tail =>
        // Define a user procedure: to name :param1 :param2 ... body... end
        tail match
          case LogoWord(procName) :: rest =>
            val (requiredParams, optionalParams, restParam, bodyStart) = collectParams(rest)
            val (body, afterEnd) = collectUntilEnd(bodyStart)
            procedures(procName.toLowerCase) =
              UserProcedure(procName.toLowerCase, requiredParams, optionalParams, restParam, body)
            (LogoNull().pos(tok.r), afterEnd)
          case _ => tok.r.error("expected procedure name after 'to'")
      case (tok @ LogoWord(s)) :: tail =>
        if s.head == '"' then (LogoWord(s.tail).pos(tok.r), tail)
        else if s.head == ':' then
          val name = s.tail.toLowerCase
          vars.get(name) match
            case Some(v) => (v, tail)
            case None    => tok.r.error(s"unknown variable '$name'")
        else if s.head.isDigit || (s.head == '-' && s != "-") then (logoNumber(s, tok.r), tail)
        else
          lookup(s) match
            case None                            => tok.r.error(s"unknown procedure, variable, or constant '$s'")
            case Some(BuiltinFunction0(_, func)) => (logoNumber(func()).pos(tok.r), tail)
            case Some(BuiltinFunction1(name, func)) =>
              val (Seq(a), rest) = evalargsn(name, 1, tail)

              (logoNumber(func(a)).pos(tok.r), rest)
            case Some(BuiltinFunction2(name, func)) =>
              val (Seq(a, b), rest) = evalargsn(name, 2, tail)

              (logoNumber(func(a, b)).pos(tok.r), rest)
            case Some(BuiltinProcedure(name, args, func)) =>
              val (vals, rest) = evalargs(name, args, tail)
              val res =
                func(this, vals) match
                  case v: LogoValue => v
                  case n: Number    => logoNumber(n)
                  case b: Boolean   => LogoBoolean(b)
                  case ()           => LogoNull()

              (res.pos(tok.r), rest)
            case Some(BuiltinVariadic(name, defaultArgs, _, func)) =>
              // Without parens, use defaultArgs count
              val (vals, rest) = evalargs(name, defaultArgs, tail)
              val res =
                func(this, vals) match
                  case v: LogoValue => v
                  case n: Number    => logoNumber(n)
                  case b: Boolean   => LogoBoolean(b)
                  case ()           => LogoNull()

              (res.pos(tok.r), rest)
            case Some(up @ UserProcedure(name, reqParams, optParams, restParam, body)) =>
              // Call user-defined procedure - without parens, only evaluate required params
              // Optional params get their default values
              val (vals, rest) = evalargs(name, reqParams.length, tail)
              val result = callUserProc(up, vals)
              (result.pos(tok.r), rest)
            case Some(v: LogoValue) => (v, tail)
            case Some(p: Procedure) => problem(tok.r, s"procedure of unknown type: '${p.name}'")
        end if

  // Collect parameter definitions: :required [:optional default] [:rest]
  private def collectParams(
      toks: Seq[LogoValue],
  ): (Seq[String], Seq[(String, LogoValue)], Option[String], Seq[LogoValue]) =
    val requiredParams = new ListBuffer[String]
    val optionalParams = new ListBuffer[(String, LogoValue)]
    var restParam: Option[String] = None

    @tailrec
    def loop(toks: Seq[LogoValue]): Seq[LogoValue] =
      toks match
        case LogoWord(s) :: rest if s.startsWith(":") =>
          // Required parameter
          requiredParams += s.tail.toLowerCase
          loop(rest)
        case LogoList(elems, _) :: rest =>
          // Optional or rest parameter: [:name] or [:name default]
          elems match
            case Seq(LogoWord(name)) if name.startsWith(":") =>
              // Rest parameter (no default value)
              restParam = Some(name.tail.toLowerCase)
              loop(rest)
            case Seq(LogoWord(name), default) if name.startsWith(":") =>
              // Optional parameter with default
              optionalParams += ((name.tail.toLowerCase, default))
              loop(rest)
            case _ => toks // Not a param, stop collecting
        case _ => toks

    val rest = loop(toks)
    (requiredParams.toSeq, optionalParams.toSeq, restParam, rest)

  // Collect tokens until we see "end"
  private def collectUntilEnd(toks: Seq[LogoValue]): (Seq[LogoValue], Seq[LogoValue]) =
    val body = new ListBuffer[LogoValue]

    @tailrec
    def loop(toks: Seq[LogoValue]): Seq[LogoValue] =
      toks match
        case LogoWord("end") :: rest => rest
        case (eoi: EOIToken) :: _    => eoi.r.error("unexpected end of input, expected 'end'")
        case tok :: rest =>
          body += tok
          loop(rest)
        case Nil => sys.error("unexpected end of tokens")

    val rest = loop(toks)
    (body.toSeq, rest)

  // Call a user-defined procedure
  private def callUserProc(proc: UserProcedure, args: Seq[LogoValue]): LogoValue =
    val UserProcedure(_, reqParams, optParams, restParam, body) = proc

    // Collect all param names for scoping
    val allParams = reqParams ++ optParams.map(_._1) ++ restParam.toSeq

    // Save current variable bindings for parameters (for proper scoping)
    val savedVars = allParams.map(p => p -> vars.get(p))

    // Bind required parameters
    reqParams.zip(args.take(reqParams.length)).foreach { case (param, arg) => vars(param) = arg }

    // Bind optional parameters (use provided args or defaults)
    val optArgs = args.drop(reqParams.length)
    optParams.zipWithIndex.foreach { case ((param, default), i) =>
      vars(param) = if i < optArgs.length then optArgs(i) else default
    }

    // Bind rest parameter to remaining args as a list
    restParam.foreach { param =>
      val restArgs = args.drop(reqParams.length + optParams.length)
      vars(param) = LogoList(restArgs, restArgs :+ EOIToken())
    }

    try
      // Execute the body (add EOI token for proper termination)
      interp(body :+ EOIToken())
    catch
      case OutputException(value) => value
      case StopException()        => LogoNull()
    finally
      // Restore previous variable bindings
      savedVars.foreach {
        case (param, Some(v)) => vars(param) = v
        case (param, None)    => vars.remove(param)
      }
