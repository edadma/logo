package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader
import io.github.edadma.dal.QuaternionDAL

import scala.annotation.tailrec
import pprint.pprintln

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.math.{Pi, cos, sin, toRadians}

// CPS evaluation result - for trampolining
sealed trait EvalResult
case class Done(value: LogoValue) extends EvalResult
case class More(thunk: () => EvalResult) extends EvalResult
// Pending procedure call - trampoline handles it to avoid stack growth
case class PendingCall(proc: UserProcedure, args: Seq[LogoValue], k: LogoValue => EvalResult) extends EvalResult
// Pending repeat loop - trampoline handles iteration to avoid stack growth
case class PendingRepeatLoop(i: Int, times: Int, body: Seq[LogoValue], k: LogoValue => EvalResult) extends EvalResult

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
  private[logo] val repcountStack          = new mutable.Stack[Int]

  // CPS: Pending control flow from output/stop
  private[logo] var pendingReturn: Option[LogoValue] = None

  // Set by output builtin - invoke this to return a value from the current procedure
  private[logo] def doOutput(value: LogoValue): Unit = pendingReturn = Some(value)

  // Set by stop builtin - invoke this to return null from the current procedure
  private[logo] def doStop(): Unit = pendingReturn = Some(LogoNull())

  // Output handler - if set, print uses this instead of println
  private[logo] var outputHandler: Option[String => Unit] = None

  def setOutputHandler(handler: String => Unit): Unit = outputHandler = Some(handler)
  def clearOutputHandler(): Unit = outputHandler = None

  private[logo] def output(s: String): Unit = outputHandler match
    case Some(handler) => handler(s)
    case None          => println(s)

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

  // Trampoline - iteratively evaluates thunks until Done
  // Uses explicit while loop to ensure no stack growth on any platform
  private def trampoline(initial: EvalResult): LogoValue =
    var current: EvalResult = initial
    while true do
      current match
        case Done(v) => return v
        case More(thunk) => current = thunk()
        case PendingCall(proc, args, k) => current = executeProcedure(proc, args, k)
        case PendingRepeatLoop(i, times, body, k) => current = executeRepeatIteration(i, times, body, k)
    throw new RuntimeException("unreachable")

  // Execute a procedure call - sets up params, evaluates body, cleans up
  private def executeProcedure(proc: UserProcedure, args: Seq[LogoValue], k: LogoValue => EvalResult): EvalResult =
    val UserProcedure(_, reqParams, optParams, restParam, body) = proc
    val allParams = reqParams ++ optParams.map(_._1) ++ restParam.toSeq
    val savedVars = allParams.map(p => p -> vars.get(p))

    // Bind parameters
    reqParams.zip(args.take(reqParams.length)).foreach { case (param, arg) => vars(param) = arg }
    val optArgs = args.drop(reqParams.length)
    optParams.zipWithIndex.foreach { case ((param, default), i) =>
      vars(param) = if i < optArgs.length then optArgs(i) else default
    }
    restParam.foreach { param =>
      val restArgs = args.drop(reqParams.length + optParams.length)
      vars(param) = LogoList(restArgs, restArgs :+ EOIToken())
    }

    // Continuation that cleans up and calls k
    // IMPORTANT: Return More() to avoid direct continuation calls that grow stack
    val bodyK: (LogoValue, Seq[LogoValue]) => EvalResult = { (result, _) =>
      val returnValue = pendingReturn.getOrElse(result)
      pendingReturn = None
      // Restore variables
      savedVars.foreach {
        case (param, Some(v)) => vars(param) = v
        case (param, None)    => vars.remove(param)
      }
      More(() => k(returnValue))
    }

    // Return body evaluation - trampoline will continue with it
    interp(body :+ EOIToken(), bodyK)

  // Execute one iteration of a repeat loop
  private def executeRepeatIteration(i: Int, times: Int, body: Seq[LogoValue], k: LogoValue => EvalResult): EvalResult =
    if i > times || pendingReturn.isDefined then
      // Done with loop or early exit via output/stop
      More(() => k(LogoNull()))
    else
      repcountStack.push(i)
      // After this iteration, continue with next iteration (or finish)
      val iterK: (LogoValue, Seq[LogoValue]) => EvalResult = { (_, _) =>
        repcountStack.pop()
        // Check for output/stop after each iteration
        if pendingReturn.isDefined then
          More(() => k(LogoNull()))
        else
          PendingRepeatLoop(i + 1, times, body, k)
      }
      interp(body :+ EOIToken(), iterK)

  def interp(input: String): LogoValue = interp(CharReader.fromString(input))

  def interp(r: CharReader): LogoValue =
    val tokens = transform(tokenize(r))
    interp(tokens)

  def interp(toks: Seq[LogoValue]): LogoValue =
    trampoline(interp(toks, (v, _) => Done(v)))

  // CPS interpreter - processes statements with continuation
  // All continuation invocations wrapped in More() to ensure trampoline handles them
  private def interp(toks: Seq[LogoValue], k: (LogoValue, Seq[LogoValue]) => EvalResult): EvalResult =
    // Check for pending output/stop BEFORE evaluating - prevents side effects from eval
    if pendingReturn.isDefined then
      More(() => k(pendingReturn.get, Seq(EOIToken()))) // Return immediately with the pending value
    else
      val (value, rest) = eval(toks)
      // Helper to continue after a control structure
      def continue(result: LogoValue): EvalResult =
        rest match
          case (eoi: EOIToken) :: _ => More(() => k(result, rest))
          case _ => More(() => interp(rest, k))

      value match
        case PendingCallMarker(proc, args) =>
          // User procedure call - return PendingCall for trampoline to handle
          PendingCall(proc, args, result => continue(result))

        case PendingIf(cond, body) =>
          if cond then
            // Execute body with continuation that continues after if
            interp(body :+ EOIToken(), (result, _) => continue(result))
          else
            continue(LogoNull())

        case PendingIfElse(cond, yesBody, noBody) =>
          val body = if cond then yesBody else noBody
          interp(body :+ EOIToken(), (result, _) => continue(result))

        case PendingRepeat(times, body) =>
          // Start repeat loop - trampoline handles iterations
          PendingRepeatLoop(1, times, body, _ => continue(LogoNull()))

        case PendingRun(code) =>
          // Parse and interpret the code with continuation
          val tokens = transform(tokenize(CharReader.fromString(code)))
          interp(tokens :+ EOIToken(), (result, _) => continue(result))

        case PendingOutput(arg) =>
          arg match
            case PendingCallMarker(proc, args) =>
              // Tail call optimization: execute the procedure and use its result as output
              PendingCall(proc, args, result => {
                pendingReturn = Some(result)
                continue(LogoNull())
              })
            case other =>
              // Regular output - resolve and set pendingReturn
              pendingReturn = Some(resolvePendingCall(other))
              continue(LogoNull())

        case _ =>
          continue(value)

  // Execute a pending call immediately and return the result
  // Used when we need the value in an expression (not at statement level)
  private def resolvePendingCall(value: LogoValue): LogoValue =
    value match
      case PendingCallMarker(proc, args) =>
        // Execute the call through the trampoline
        trampoline(PendingCall(proc, args, v => Done(v)))
      case v => v

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
        // Resolve pending calls since we need the actual value
        buf += resolvePendingCall(arg)
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
          // Resolve pending calls since we need the actual value
          buf += resolvePendingCall(arg)
          loop(rest)

    val rest = loop(toks)
    if buf.size < minArgs then
      problem(null, s"'$name' requires at least $minArgs argument(s), got ${buf.size}")
    (buf.toSeq, rest)

  def eval(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) = evalComparison(toks)

  private def evalComparison(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
    val (left0, rest) = evalAdditive(toks)
    rest match
      case LogoWord(op @ ("=" | "<>" | "<" | ">" | "<=" | ">=")) :: tail =>
        val (right0, rest2) = evalAdditive(tail)
        val left = resolvePendingCall(left0)
        val right = resolvePendingCall(right0)
        val result = op match
          case "="  => LogoBoolean(left == right)
          case "<>" => LogoBoolean(left != right)
          case "<"  => LogoBoolean(QuaternionDAL.relate("<", number(left), number(right)))
          case ">"  => LogoBoolean(QuaternionDAL.relate(">", number(left), number(right)))
          case "<=" => LogoBoolean(QuaternionDAL.relate("<=", number(left), number(right)))
          case ">=" => LogoBoolean(QuaternionDAL.relate(">=", number(left), number(right)))
        (result.pos(left.r), rest2)
      case _ => (left0, rest)

  private def evalAdditive(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
    @tailrec
    def loop(left: LogoValue, toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
      toks match
        case LogoWord(op @ ("+" | "-")) :: tail =>
          val (right0, rest) = evalMultiplicative(tail)
          val left1 = resolvePendingCall(left)
          val right = resolvePendingCall(right0)
          val result = logoNumber(QuaternionDAL.compute(op, number(left1), number(right)))
          loop(result, rest)
        case _ => (left, toks)

    val (left, rest) = evalMultiplicative(toks)
    loop(left, rest)

  private def evalMultiplicative(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
    @tailrec
    def loop(left: LogoValue, toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
      toks match
        case LogoWord("*") :: tail =>
          val (right0, rest) = evalPower(tail)
          val left1 = resolvePendingCall(left)
          val right = resolvePendingCall(right0)
          val result = logoNumber(QuaternionDAL.compute("*", number(left1), number(right)))
          loop(result, rest)
        case LogoWord("/") :: tail =>
          // Exact arithmetic division
          val (right0, rest) = evalPower(tail)
          val left1 = resolvePendingCall(left)
          val right = resolvePendingCall(right0)
          val result = logoNumber(QuaternionDAL.compute("/", number(left1), number(right)))
          loop(result, rest)
        case LogoWord("\\") :: tail =>
          // Float division - always returns Double
          val (right0, rest) = evalPower(tail)
          val left1 = resolvePendingCall(left)
          val right = resolvePendingCall(right0)
          val result = logoNumber(number(left1).doubleValue / number(right).doubleValue)
          loop(result, rest)
        case LogoWord("//") :: tail =>
          // Floor division - returns integer
          val (right0, rest) = evalPower(tail)
          val left1 = resolvePendingCall(left)
          val right = resolvePendingCall(right0)
          val result = logoNumber((math.floor(number(left1).doubleValue / number(right).doubleValue).toLong).toDouble)
          loop(result, rest)
        case _ => (left, toks)

    val (left, rest) = evalPower(toks)
    loop(left, rest)

  // Power is right-associative: 2^3^2 = 2^(3^2) = 2^9 = 512
  private def evalPower(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
    val (left0, rest) = evalPrimary(toks)
    rest match
      case LogoWord("^") :: tail =>
        val (right0, rest2) = evalPower(tail) // right-associative: recurse instead of loop
        val left = resolvePendingCall(left0)
        val right = resolvePendingCall(right0)
        val result = logoNumber(QuaternionDAL.compute("^", number(left), number(right)))
        (result.pos(left.r), rest2)
      case _ => (left0, rest)

  private def evalPrimary(toks: Seq[LogoValue]): (LogoValue, Seq[LogoValue]) =
    toks match
      case (eoi @ EOIToken()) :: _                         => (LogoNull().pos(eoi.r), Seq(eoi))
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
                (PendingCallMarker(up, args).pos(tok.r), rest2)
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
      // Control structures as special forms - return pending markers for CPS handling
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "if" || w.toLowerCase == "si" =>
        val (Seq(condVal, bodyVal), rest) = evalargs("if", 2, tail)
        val cond = boolean(resolvePendingCall(condVal))
        val body = list(bodyVal)
        (PendingIf(cond, body).pos(tok.r), rest)
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "ifelse" || w.toLowerCase == "siou" =>
        val (Seq(condVal, yesVal, noVal), rest) = evalargs("ifelse", 3, tail)
        val cond = boolean(resolvePendingCall(condVal))
        val yesBody = list(yesVal)
        val noBody = list(noVal)
        (PendingIfElse(cond, yesBody, noBody).pos(tok.r), rest)
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "repeat" || w.toLowerCase == "repete" =>
        val (Seq(timesVal, bodyVal), rest) = evalargs("repeat", 2, tail)
        val times = number(resolvePendingCall(timesVal)).intValue
        val body = list(bodyVal)
        (PendingRepeat(times, body).pos(tok.r), rest)
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "run" =>
        val (Seq(codeVal), rest) = evalargs("run", 1, tail)
        val code = resolvePendingCall(codeVal).toString
        (PendingRun(code).pos(tok.r), rest)
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "output" || w.toLowerCase == "op" =>
        // output is a special form to enable tail call optimization
        // Don't resolve the argument - if it's a pending call, we'll do a tail call
        val (arg, rest) = eval(tail)
        (PendingOutput(arg).pos(tok.r), rest)
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
              (PendingCallMarker(up, vals).pos(tok.r), rest)
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
