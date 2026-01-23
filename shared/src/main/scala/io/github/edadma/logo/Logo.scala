package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader
import io.github.edadma.dal.QuaternionDAL

import scala.annotation.tailrec
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
// Pending for loop - trampoline handles iteration
case class PendingForLoop(varName: String, current: Double, end: Double, step: Double, body: Seq[LogoValue], k: LogoValue => EvalResult) extends EvalResult
// Pending while loop - trampoline handles iteration
case class PendingWhileLoop(conditionCode: Seq[LogoValue], body: Seq[LogoValue], k: LogoValue => EvalResult) extends EvalResult
// Pending forever loop - runs until stop
case class PendingForeverLoop(body: Seq[LogoValue], k: LogoValue => EvalResult) extends EvalResult
// Pending do.while loop - runs body first, then checks condition
case class PendingDoWhileLoop(body: Seq[LogoValue], conditionCode: Seq[LogoValue], k: LogoValue => EvalResult) extends EvalResult

// CPS continuation type aliases
type EvalK = (LogoValue, Seq[LogoValue]) => EvalResult
type ArgsK = (Seq[LogoValue], Seq[LogoValue]) => EvalResult

// Screen boundary modes
sealed trait ScreenMode
case object WindowMode extends ScreenMode  // No boundaries, turtle can go anywhere (default)
case object FenceMode extends ScreenMode   // Error if turtle tries to leave bounds
case object WrapMode extends ScreenMode    // Turtle wraps around to opposite side

// Pen modes
sealed trait PenMode
case object PaintMode extends PenMode   // Normal drawing
case object EraseMode extends PenMode   // Erase (draw in background color)
case object ReverseMode extends PenMode // XOR drawing (invert colors)

abstract class Logo:
  def event(): Unit

  // Input methods - override in platform-specific implementations
  def readLine(): String = scala.io.StdIn.readLine()
  def readChar(): Int = System.in.read()

  private[logo] var x: Double              = 0
  private[logo] var y: Double              = 0
  private[logo] var heading: Double        = Pi / 2
  private[logo] var color: (Int, Int, Int) = colorMap("black")
  private[logo] var defaultColor: (Int, Int, Int) = colorMap("black")
  private[logo] var backgroundColor: (Int, Int, Int) = colorMap("white")
  private[logo] var pen: Boolean           = true
  private[logo] var penMode: PenMode       = PaintMode
  private[logo] var width: Double          = 1
  private[logo] var show: Boolean          = true
  private[logo] val draws                  = new ListBuffer[Draw]
  private[logo] val vars                   = new mutable.HashMap[String, LogoValue]
  private[logo] val procedures             = new mutable.HashMap[String, UserProcedure]
  private[logo] val repcountStack          = new mutable.Stack[Int]

  // Test result for test/iftrue/iffalse - stack to support nested procedures
  private[logo] val testResultStack        = new mutable.Stack[Boolean]

  // Local variable support: stack of frames, each frame is a list of (varname, saved value)
  private[logo] val localVarsStack         = new mutable.Stack[mutable.ListBuffer[(String, Option[LogoValue])]]

  // Declare a variable as local to the current procedure
  private[logo] def declareLocal(name: String): Unit =
    if localVarsStack.nonEmpty then
      val frame = localVarsStack.top
      // Only add if not already declared local in this frame
      if !frame.exists(_._1 == name) then
        frame += ((name, vars.get(name)))

  // Screen boundary settings
  private[logo] var screenMode: ScreenMode = WindowMode
  private[logo] var screenBounds: (Double, Double, Double, Double) = (-500, -500, 500, 500)  // (minX, minY, maxX, maxY)

  def setScreenBounds(minX: Double, minY: Double, maxX: Double, maxY: Double): Unit =
    screenBounds = (minX, minY, maxX, maxY)

  // Apply screen mode to a new position, returns adjusted (x, y) or throws error
  private[logo] def applyScreenMode(newX: Double, newY: Double): (Double, Double) =
    val (minX, minY, maxX, maxY) = screenBounds
    val width = maxX - minX
    val height = maxY - minY

    screenMode match
      case WindowMode => (newX, newY)
      case FenceMode =>
        if newX < minX || newX > maxX || newY < minY || newY > maxY then
          problem(null, s"Turtle out of bounds at ($newX, $newY)")
        (newX, newY)
      case WrapMode =>
        val wrappedX =
          if newX < minX then maxX - (minX - newX) % width
          else if newX > maxX then minX + (newX - maxX) % width
          else newX
        val wrappedY =
          if newY < minY then maxY - (minY - newY) % height
          else if newY > maxY then minY + (newY - maxY) % height
          else newY
        (wrappedX, wrappedY)

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

  private[logo] def outputNoNewline(s: String): Unit = outputHandler match
    case Some(handler) => handler(s)
    case None          => print(s)

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
    color = defaultColor
    pen = true
    penMode = PaintMode
    width = 1

  def clean(): Unit =
    draws.clear()

  def setDefaultColor(c: (Int, Int, Int)): Unit =
    defaultColor = c
    color = c

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
        case PendingForLoop(varName, current_, end, step, body, k) => current = executeForIteration(varName, current_, end, step, body, k)
        case PendingWhileLoop(conditionCode, body, k) => current = executeWhileIteration(conditionCode, body, k)
        case PendingForeverLoop(body, k) => current = executeForeverIteration(body, k)
        case PendingDoWhileLoop(body, conditionCode, k) => current = executeDoWhileIteration(body, conditionCode, k)
    throw new RuntimeException("unreachable")

  // Execute a procedure call - sets up params, evaluates body, cleans up
  private def executeProcedure(proc: UserProcedure, args: Seq[LogoValue], k: LogoValue => EvalResult): EvalResult =
    val UserProcedure(_, reqParams, optParams, restParam, body) = proc
    val allParams = reqParams ++ optParams.map(_._1) ++ restParam.toSeq
    val savedVars = allParams.map(p => p -> vars.get(p))

    // Push a new frame for local variables
    localVarsStack.push(new mutable.ListBuffer[(String, Option[LogoValue])])

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

      // Restore local variables declared with local/localmake
      val localFrame = localVarsStack.pop()
      localFrame.foreach {
        case (name, Some(v)) => vars(name) = v
        case (name, None)    => vars.remove(name)
      }

      // Restore parameter variables
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

  // Execute one iteration of a for loop
  private def executeForIteration(varName: String, current: Double, end: Double, step: Double, body: Seq[LogoValue], k: LogoValue => EvalResult): EvalResult =
    val done = if step > 0 then current > end else current < end
    if done || pendingReturn.isDefined then
      // Done with loop or early exit via output/stop
      More(() => k(LogoNull()))
    else
      // Save and set loop variable
      val savedVar = vars.get(varName)
      vars(varName) = LogoNumber(current)
      // After this iteration, continue with next iteration (or finish)
      val iterK: (LogoValue, Seq[LogoValue]) => EvalResult = { (_, _) =>
        // Restore variable if it existed before, otherwise remove
        savedVar match
          case Some(v) => vars(varName) = v
          case None    => vars.remove(varName)
        // Check for output/stop after each iteration
        if pendingReturn.isDefined then
          More(() => k(LogoNull()))
        else
          PendingForLoop(varName, current + step, end, step, body, k)
      }
      interp(body :+ EOIToken(), iterK)

  // Execute while loop - evaluate condition, then body if true
  private def executeWhileIteration(conditionCode: Seq[LogoValue], body: Seq[LogoValue], k: LogoValue => EvalResult): EvalResult =
    if pendingReturn.isDefined then
      More(() => k(LogoNull()))
    else
      // Evaluate condition
      interp(conditionCode :+ EOIToken(), { (condResult, _) =>
        resolveThenContinue(condResult, { condVal =>
          if !boolean(condVal) then
            // Condition false, done with loop
            More(() => k(LogoNull()))
          else
            // Condition true, execute body then loop
            interp(body :+ EOIToken(), { (_, _) =>
              if pendingReturn.isDefined then
                More(() => k(LogoNull()))
              else
                PendingWhileLoop(conditionCode, body, k)
            })
        })
      })

  // Execute one iteration of a forever loop
  private def executeForeverIteration(body: Seq[LogoValue], k: LogoValue => EvalResult): EvalResult =
    if pendingReturn.isDefined then
      More(() => k(LogoNull()))
    else
      // Execute body then loop again
      interp(body :+ EOIToken(), { (_, _) =>
        if pendingReturn.isDefined then
          More(() => k(LogoNull()))
        else
          PendingForeverLoop(body, k)
      })

  // Execute one iteration of a do.while loop - runs body first, then checks condition
  private def executeDoWhileIteration(body: Seq[LogoValue], conditionCode: Seq[LogoValue], k: LogoValue => EvalResult): EvalResult =
    if pendingReturn.isDefined then
      More(() => k(LogoNull()))
    else
      // Execute body first
      interp(body :+ EOIToken(), { (_, _) =>
        if pendingReturn.isDefined then
          More(() => k(LogoNull()))
        else
          // Then check condition
          interp(conditionCode :+ EOIToken(), { (condResult, _) =>
            if pendingReturn.isDefined then
              More(() => k(LogoNull()))
            else if boolean(condResult) then
              PendingDoWhileLoop(body, conditionCode, k) // Continue looping
            else
              More(() => k(LogoNull())) // Done
          })
      })

  // Execute case statement - find matching clause and execute it
  private def executeCase(testVal: LogoValue, clauses: List[LogoValue], k: LogoValue => EvalResult): EvalResult =
    clauses match
      case Nil => More(() => k(LogoNull())) // No match found
      case clause :: rest =>
        val clauseList = list(clause)
        if clauseList.isEmpty then
          More(() => k(LogoNull()))
        else
          val selector = clauseList.head
          val body = clauseList.tail
          // Check if selector is "else" or matches the test value
          val isElse = selector match
            case LogoWord(w) => w.toLowerCase == "else"
            case _ => false
          val matches = isElse || (selector match
            case LogoList(values, _) => values.exists(v => valuesEqual(v, testVal))
            case v => valuesEqual(v, testVal)
          )
          if matches then
            if body.isEmpty then More(() => k(LogoNull()))
            else interp(body :+ EOIToken(), (result, _) => More(() => k(result)))
          else
            executeCase(testVal, rest, k)

  // Execute cond statement - evaluate conditions until one is true
  private def executeCond(clauses: List[LogoValue], k: LogoValue => EvalResult): EvalResult =
    clauses match
      case Nil => More(() => k(LogoNull())) // No match found
      case clause :: rest =>
        val clauseList = list(clause)
        if clauseList.isEmpty then
          executeCond(rest, k)
        else
          val conditionOrElse = clauseList.head
          val body = clauseList.tail
          // Check if this is an "else" clause
          val isElse = conditionOrElse match
            case LogoWord(w) => w.toLowerCase == "else"
            case _ => false
          if isElse then
            if body.isEmpty then More(() => k(LogoNull()))
            else interp(body :+ EOIToken(), (result, _) => More(() => k(result)))
          else
            // Evaluate the condition - it can be an expression list
            val condCode = conditionOrElse match
              case LogoList(elems, _) => elems
              case v => Seq(v)
            interp(condCode :+ EOIToken(), { (condResult, _) =>
              if boolean(condResult) then
                if body.isEmpty then More(() => k(LogoNull()))
                else interp(body :+ EOIToken(), (result, _) => More(() => k(result)))
              else
                executeCond(rest, k)
            })

  // Helper to check if two Logo values are equal
  private def valuesEqual(a: LogoValue, b: LogoValue): Boolean =
    (a, b) match
      case (LogoNumber(n1), LogoNumber(n2)) => n1.doubleValue == n2.doubleValue
      case (LogoWord(w1), LogoWord(w2)) => w1.equalsIgnoreCase(w2)
      case (LogoBoolean(b1), LogoBoolean(b2)) => b1 == b2
      case (LogoList(l1, _), LogoList(l2, _)) => l1.length == l2.length && l1.zip(l2).forall { case (x, y) => valuesEqual(x, y) }
      case _ => a.toString == b.toString

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
      // Use CPS evaluation - continuation receives (value, rest)
      evalCPS(toks, { (value, rest) =>
        // Helper to continue after a statement
        def continue(result: LogoValue): EvalResult =
          rest match
            case (eoi: EOIToken) :: _ => More(() => k(result, rest))
            case _                    => More(() => interp(rest, k))

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

          case PendingFor(varName, start, end, step, body) =>
            // Start for loop - trampoline handles iterations
            PendingForLoop(varName, start, end, step, body, _ => continue(LogoNull()))

          case PendingWhile(conditionCode, body) =>
            // Start while loop - trampoline handles iterations
            PendingWhileLoop(conditionCode, body, _ => continue(LogoNull()))

          case PendingForever(body) =>
            // Start forever loop - trampoline handles iterations until stop
            PendingForeverLoop(body, _ => continue(LogoNull()))

          case PendingDoWhile(body, conditionCode) =>
            // Start do.while loop - run body first, then check condition
            PendingDoWhileLoop(body, conditionCode, _ => continue(LogoNull()))

          case PendingCase(testVal, clauses) =>
            // Process case clauses
            executeCase(testVal, clauses.toList, result => continue(result))

          case PendingCond(clauses) =>
            // Process cond clauses
            executeCond(clauses.toList, result => continue(result))

          case PendingRun(code) =>
            // Parse and interpret the code with continuation
            val tokens = transform(tokenize(CharReader.fromString(code)))
            interp(tokens :+ EOIToken(), (result, _) => continue(result))

          case PendingRunResult(code) =>
            // Run code and wrap any output in a list
            val savedPendingReturn = pendingReturn
            pendingReturn = None
            val tokens = transform(tokenize(CharReader.fromString(code)))
            interp(tokens :+ EOIToken(), (interpResult, _) => {
              // Check both the interpreted result and pendingReturn
              // Procedures return their output through interpResult (via continuation)
              // Direct 'output' statements set pendingReturn
              val outputVal = pendingReturn match
                case Some(v) => Some(v)
                case None =>
                  interpResult match
                    case LogoNull() => None
                    case v => Some(v)
              val result = outputVal match
                case Some(v) =>
                  val elems = Seq(v)
                  LogoList(elems, elems :+ EOIToken())
                case None =>
                  LogoList(Seq.empty, Seq(EOIToken()))
              pendingReturn = savedPendingReturn
              continue(result)
            })

          case PendingOutput(arg) =>
            arg match
              case PendingCallMarker(proc, args) =>
                // Tail call optimization: execute the procedure and use its result as output
                PendingCall(proc, args, result => {
                  pendingReturn = Some(result)
                  continue(LogoNull())
                })
              case other =>
                // Regular output - resolve through CPS
                resolveThenContinue(other, { resolved =>
                  pendingReturn = Some(resolved)
                  continue(LogoNull())
                })

          case _ =>
            continue(value)
      })

  // CPS helper: resolve a value through continuation, handling pending operations
  private def resolveThenContinue(value: LogoValue, k: LogoValue => EvalResult): EvalResult =
    value match
      case PendingCallMarker(proc, args) =>
        PendingCall(proc, args, k)
      case PendingIfElse(cond, yesBody, noBody) =>
        // Evaluate the appropriate branch and continue with result
        val body = if cond then yesBody else noBody
        interp(body :+ EOIToken(), (result, _) => resolveThenContinue(result, k))
      case PendingIf(cond, body) =>
        if cond then interp(body :+ EOIToken(), (result, _) => resolveThenContinue(result, k))
        else More(() => k(LogoNull()))
      case PendingRun(code) =>
        val tokens = transform(tokenize(CharReader.fromString(code)))
        interp(tokens :+ EOIToken(), (result, _) => resolveThenContinue(result, k))
      case PendingRunResult(code) =>
        // Run code and wrap any output in a list
        val savedPendingReturn = pendingReturn
        pendingReturn = None
        val tokens = transform(tokenize(CharReader.fromString(code)))
        interp(tokens :+ EOIToken(), (interpResult, _) => {
          // Check both the interpreted result and pendingReturn
          val outputVal = pendingReturn match
            case Some(v) => Some(v)
            case None =>
              interpResult match
                case LogoNull() => None
                case v => Some(v)
          val result = outputVal match
            case Some(v) =>
              val elems = Seq(v)
              LogoList(elems, elems :+ EOIToken())
            case None =>
              LogoList(Seq.empty, Seq(EOIToken()))
          pendingReturn = savedPendingReturn
          More(() => k(result))
        })
      case v =>
        More(() => k(v))

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

  // ============================================================================
  // CPS Evaluation - Argument Collection
  // ============================================================================

  // Collect arguments through continuations
  private def evalargsCPS(
      name: String,
      count: Int,
      toks: Seq[LogoValue],
      acc: Seq[LogoValue],
      k: ArgsK,
  ): EvalResult =
    if count == 0 then
      More(() => k(acc, toks))
    else if toks.isEmpty || toks.head.isInstanceOf[EOIToken] then
      if toks.nonEmpty then toks.head.r.error(s"unexpected end of input while evaluating argument(s) for '$name'")
      else problem(null, s"unexpected end of input while evaluating argument(s) for '$name'")
    else
      evalCPS(toks, { (arg, rest) =>
        resolveThenContinue(arg, { value =>
          More(() => evalargsCPS(name, count - 1, rest, acc :+ value, k))
        })
      })

  // Collect variadic arguments until closing paren
  private def evalArgsUntilParenCPS(
      name: String,
      minArgs: Int,
      toks: Seq[LogoValue],
      acc: Seq[LogoValue],
      k: ArgsK,
  ): EvalResult =
    toks match
      case LogoWord(")") :: rest =>
        if acc.size < minArgs then
          problem(null, s"'$name' requires at least $minArgs argument(s), got ${acc.size}")
        More(() => k(acc, rest))
      case (eoi: EOIToken) :: _ =>
        eoi.r.error(s"expected closing parenthesis for variadic call to '$name'")
      case _ =>
        evalCPS(toks, { (arg, rest) =>
          resolveThenContinue(arg, { value =>
            More(() => evalArgsUntilParenCPS(name, minArgs, rest, acc :+ value, k))
          })
        })

  // ============================================================================
  // CPS Evaluation - Expression Parsers
  // ============================================================================

  // Top-level CPS evaluation
  def evalCPS(toks: Seq[LogoValue], k: EvalK): EvalResult =
    evalComparisonCPS(toks, k)

  // Comparison: = <> < > <= >=
  private def evalComparisonCPS(toks: Seq[LogoValue], k: EvalK): EvalResult =
    evalAdditiveCPS(toks, { (left0, rest) =>
      rest match
        case LogoWord(op @ ("=" | "<>" | "<" | ">" | "<=" | ">=")) :: tail =>
          evalAdditiveCPS(tail, { (right0, rest2) =>
            resolveThenContinue(left0, { left =>
              resolveThenContinue(right0, { right =>
                val result = op match
                  case "="  => LogoBoolean(left == right)
                  case "<>" => LogoBoolean(left != right)
                  case "<"  => LogoBoolean(QuaternionDAL.relate("<", number(left), number(right)))
                  case ">"  => LogoBoolean(QuaternionDAL.relate(">", number(left), number(right)))
                  case "<=" => LogoBoolean(QuaternionDAL.relate("<=", number(left), number(right)))
                  case ">=" => LogoBoolean(QuaternionDAL.relate(">=", number(left), number(right)))
                More(() => k(result.pos(left.r), rest2))
              })
            })
          })
        case _ =>
          More(() => k(left0, rest))
    })

  // Additive: + -
  private def evalAdditiveCPS(toks: Seq[LogoValue], k: EvalK): EvalResult =
    evalMultiplicativeCPS(toks, { (left, rest) =>
      evalAdditiveContinue(left, rest, k)
    })

  private def evalAdditiveContinue(left: LogoValue, toks: Seq[LogoValue], k: EvalK): EvalResult =
    toks match
      case LogoWord(op @ ("+" | "-")) :: tail =>
        evalMultiplicativeCPS(tail, { (right0, rest) =>
          resolveThenContinue(left, { leftVal =>
            resolveThenContinue(right0, { rightVal =>
              val result = logoNumber(QuaternionDAL.compute(op, number(leftVal), number(rightVal)))
              More(() => evalAdditiveContinue(result.pos(leftVal.r), rest, k))
            })
          })
        })
      case _ =>
        More(() => k(left, toks))

  // Multiplicative: * / \ //
  private def evalMultiplicativeCPS(toks: Seq[LogoValue], k: EvalK): EvalResult =
    evalPowerCPS(toks, { (left, rest) =>
      evalMultiplicativeContinue(left, rest, k)
    })

  private def evalMultiplicativeContinue(left: LogoValue, toks: Seq[LogoValue], k: EvalK): EvalResult =
    toks match
      case LogoWord("*") :: tail =>
        evalPowerCPS(tail, { (right0, rest) =>
          resolveThenContinue(left, { leftVal =>
            resolveThenContinue(right0, { rightVal =>
              val result = logoNumber(QuaternionDAL.compute("*", number(leftVal), number(rightVal)))
              More(() => evalMultiplicativeContinue(result.pos(leftVal.r), rest, k))
            })
          })
        })
      case LogoWord("/") :: tail =>
        evalPowerCPS(tail, { (right0, rest) =>
          resolveThenContinue(left, { leftVal =>
            resolveThenContinue(right0, { rightVal =>
              val result = logoNumber(QuaternionDAL.compute("/", number(leftVal), number(rightVal)))
              More(() => evalMultiplicativeContinue(result.pos(leftVal.r), rest, k))
            })
          })
        })
      case LogoWord("\\") :: tail =>
        evalPowerCPS(tail, { (right0, rest) =>
          resolveThenContinue(left, { leftVal =>
            resolveThenContinue(right0, { rightVal =>
              val result = logoNumber(number(leftVal).doubleValue / number(rightVal).doubleValue)
              More(() => evalMultiplicativeContinue(result.pos(leftVal.r), rest, k))
            })
          })
        })
      case LogoWord("//") :: tail =>
        evalPowerCPS(tail, { (right0, rest) =>
          resolveThenContinue(left, { leftVal =>
            resolveThenContinue(right0, { rightVal =>
              val result = logoNumber((math.floor(number(leftVal).doubleValue / number(rightVal).doubleValue).toLong).toDouble)
              More(() => evalMultiplicativeContinue(result.pos(leftVal.r), rest, k))
            })
          })
        })
      case _ =>
        More(() => k(left, toks))

  // Power: ^ (right-associative)
  private def evalPowerCPS(toks: Seq[LogoValue], k: EvalK): EvalResult =
    evalPrimaryCPS(toks, { (left0, rest) =>
      rest match
        case LogoWord("^") :: tail =>
          evalPowerCPS(tail, { (right0, rest2) =>
            resolveThenContinue(left0, { left =>
              resolveThenContinue(right0, { right =>
                val result = logoNumber(QuaternionDAL.compute("^", number(left), number(right)))
                More(() => k(result.pos(left.r), rest2))
              })
            })
          })
        case _ =>
          More(() => k(left0, rest))
    })

  // Primary expressions - CPS version
  private def evalPrimaryCPS(toks: Seq[LogoValue], k: EvalK): EvalResult =
    toks match
      case (eoi @ EOIToken()) :: _ =>
        More(() => k(LogoNull().pos(eoi.r), Seq(eoi)))

      case (v: (LogoNumber | LogoList | LogoNull)) :: tail =>
        More(() => k(v, tail))

      case (tok @ LogoWord("true" | "false")) :: tail =>
        More(() => k(LogoBoolean(tok.toString == "true").pos(tok.r), tail))

      case (tok @ LogoWord("null")) :: tail =>
        More(() => k(LogoNull().pos(tok.r), tail))

      case (tok @ LogoWord("(")) :: tail =>
        // Check if this is a variadic procedure call or expression grouping
        tail match
          case (procTok @ LogoWord(procName)) :: rest if !procName.head.isDigit && procName.head != '"' && procName.head != ':' =>
            lookup(procName) match
              case Some(BuiltinVariadic(name, _, minArgs, func)) =>
                evalArgsUntilParenCPS(name, minArgs, rest, Seq.empty, { (args, rest2) =>
                  val res = func(this, args) match
                    case v: LogoValue => v
                    case n: Number    => logoNumber(n)
                    case b: Boolean   => LogoBoolean(b)
                    case ()           => LogoUnit
                  More(() => k(res.pos(tok.r), rest2))
                })
              case Some(up @ UserProcedure(name, reqParams, optParams, restParam, _))
                  if optParams.nonEmpty || restParam.isDefined =>
                evalArgsUntilParenCPS(name, reqParams.length, rest, Seq.empty, { (args, rest2) =>
                  More(() => k(PendingCallMarker(up, args).pos(tok.r), rest2))
                })
              case _ =>
                // Not a variadic procedure - treat as expression grouping
                evalCPS(tail, { (value, rest2) =>
                  rest2 match
                    case LogoWord(")") :: rest3 => More(() => k(value.pos(tok.r), rest3))
                    case _                      => tok.r.error("expected closing parenthesis")
                })
          case _ =>
            // Expression grouping
            evalCPS(tail, { (value, rest) =>
              rest match
                case LogoWord(")") :: rest2 => More(() => k(value.pos(tok.r), rest2))
                case _                      => tok.r.error("expected closing parenthesis")
            })

      // Control structures as special forms
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "if" || w.toLowerCase == "si" =>
        evalargsCPS("if", 2, tail, Seq.empty, { (args, rest) =>
          resolveThenContinue(args(0), { condVal =>
            val cond = boolean(condVal)
            val body = list(args(1))
            More(() => k(PendingIf(cond, body).pos(tok.r), rest))
          })
        })

      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "ifelse" || w.toLowerCase == "siou" =>
        evalargsCPS("ifelse", 3, tail, Seq.empty, { (args, rest) =>
          resolveThenContinue(args(0), { condVal =>
            val cond = boolean(condVal)
            val yesBody = list(args(1))
            val noBody = list(args(2))
            More(() => k(PendingIfElse(cond, yesBody, noBody).pos(tok.r), rest))
          })
        })

      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "repeat" || w.toLowerCase == "repete" =>
        evalargsCPS("repeat", 2, tail, Seq.empty, { (args, rest) =>
          resolveThenContinue(args(0), { timesVal =>
            val times = number(timesVal).intValue
            val body = list(args(1))
            More(() => k(PendingRepeat(times, body).pos(tok.r), rest))
          })
        })

      // UCB Logo for: for [var start end step] [body]
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "for" =>
        evalargsCPS("for", 2, tail, Seq.empty, { (args, rest) =>
          val controlList = list(args(0))
          val body = list(args(1))

          // Helper to evaluate a value from the control list
          def evalControlVal(v: LogoValue): Double =
            v match
              case LogoNumber(n) => n.doubleValue
              case LogoWord(s) =>
                // Try to parse as number first
                try
                  s.toDouble
                catch
                  case _: NumberFormatException =>
                    if s.startsWith(":") then
                      // Variable reference
                      val varName = s.tail.toLowerCase
                      vars.get(varName) match
                        case Some(n) => number(n).doubleValue
                        case None => problem(v.r, s"unknown variable '$varName'")
                    else
                      // Try to evaluate as expression
                      number(interp(s"print $s")).doubleValue // This is a hack, we need better approach
              case other => problem(v.r, s"expected a number in for control list, got ${other.getClass.getSimpleName}")

          // Parse control list - handle negative numbers which may be tokenized as two tokens
          // [i 1 5] -> 3 elements, [i 1 5 1] -> 4 elements, [i 1 5 - 1] -> 5 elements (negative step)
          val (varName, startNum, endNum, stepNum) = controlList match
            case Seq(LogoWord(v), s, e, LogoWord("-"), step) =>
              // Negative step: [var start end - step]
              (v.toLowerCase, evalControlVal(s), evalControlVal(e), -evalControlVal(step))
            case Seq(LogoWord(v), s, e, step) =>
              (v.toLowerCase, evalControlVal(s), evalControlVal(e), evalControlVal(step))
            case Seq(LogoWord(v), s, e) =>
              val sv = evalControlVal(s)
              val ev = evalControlVal(e)
              // Default step: 1 if start <= end, -1 otherwise
              (v.toLowerCase, sv, ev, if sv <= ev then 1.0 else -1.0)
            case other =>
              tok.r.error(s"'for' control list must be [var start end] or [var start end step], got ${other.length} elements: ${other.map(_.getClass.getSimpleName).mkString(", ")}")

          More(() => k(PendingFor(varName, startNum, endNum, stepNum, body).pos(tok.r), rest))
        })

      // UCB Logo while: while [condition] [body]
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "while" =>
        evalargsCPS("while", 2, tail, Seq.empty, { (args, rest) =>
          val conditionCode = list(args(0))
          val body = list(args(1))
          More(() => k(PendingWhile(conditionCode, body).pos(tok.r), rest))
        })

      // UCB Logo until: until [condition] [body] - loop while condition is FALSE
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "until" =>
        evalargsCPS("until", 2, tail, Seq.empty, { (args, rest) =>
          val conditionCode = list(args(0))
          val body = list(args(1))
          // Wrap condition with NOT to invert it
          val invertedCondition = Seq(LogoWord("not")) ++ conditionCode
          More(() => k(PendingWhile(invertedCondition, body).pos(tok.r), rest))
        })

      // UCB Logo forever: forever [body] - loop until stop is called
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "forever" =>
        evalargsCPS("forever", 1, tail, Seq.empty, { (args, rest) =>
          val body = list(args(0))
          More(() => k(PendingForever(body).pos(tok.r), rest))
        })

      // do.while: do.while [body] [condition] - run body first, then loop while condition is true
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "do.while" =>
        evalargsCPS("do.while", 2, tail, Seq.empty, { (args, rest) =>
          val body = list(args(0))
          val conditionCode = list(args(1))
          More(() => k(PendingDoWhile(body, conditionCode).pos(tok.r), rest))
        })

      // do.until: do.until [body] [condition] - run body first, then loop while condition is false
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "do.until" =>
        evalargsCPS("do.until", 2, tail, Seq.empty, { (args, rest) =>
          val body = list(args(0))
          val conditionCode = list(args(1))
          // Wrap condition with NOT to invert it
          val invertedCondition = Seq(LogoWord("not")) ++ conditionCode
          More(() => k(PendingDoWhile(body, invertedCondition).pos(tok.r), rest))
        })

      // iftrue / ift: run body if test was true
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "iftrue" || w.toLowerCase == "ift" =>
        evalargsCPS("iftrue", 1, tail, Seq.empty, { (args, rest) =>
          val body = list(args(0))
          if testResultStack.isEmpty then
            tok.r.error("iftrue without test")
          val cond = testResultStack.top
          More(() => k(PendingIf(cond, body).pos(tok.r), rest))
        })

      // iffalse / iff: run body if test was false
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "iffalse" || w.toLowerCase == "iff" =>
        evalargsCPS("iffalse", 1, tail, Seq.empty, { (args, rest) =>
          val body = list(args(0))
          if testResultStack.isEmpty then
            tok.r.error("iffalse without test")
          val cond = !testResultStack.top
          More(() => k(PendingIf(cond, body).pos(tok.r), rest))
        })

      // case: case value [[values...] instructions] ...
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "case" =>
        evalargsCPS("case", 2, tail, Seq.empty, { (args, rest) =>
          resolveThenContinue(args(0), { testVal =>
            val clauses = list(args(1))
            More(() => k(PendingCase(testVal, clauses).pos(tok.r), rest))
          })
        })

      // cond: cond [[condition] instructions] ...
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "cond" =>
        evalargsCPS("cond", 1, tail, Seq.empty, { (args, rest) =>
          val clauses = list(args(0))
          More(() => k(PendingCond(clauses).pos(tok.r), rest))
        })

      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "run" =>
        evalargsCPS("run", 1, tail, Seq.empty, { (args, rest) =>
          resolveThenContinue(args(0), { codeVal =>
            val code = codeVal.toString
            More(() => k(PendingRun(code).pos(tok.r), rest))
          })
        })

      // runresult: run code and wrap output in a list (or empty list if no output)
      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "runresult" =>
        evalargsCPS("runresult", 1, tail, Seq.empty, { (args, rest) =>
          resolveThenContinue(args(0), { codeVal =>
            val code = codeVal.toString
            More(() => k(PendingRunResult(code).pos(tok.r), rest))
          })
        })

      case (tok @ LogoWord(w)) :: tail if w.toLowerCase == "output" || w.toLowerCase == "op" =>
        // output is a special form - don't resolve the argument for tail call optimization
        evalCPS(tail, { (arg, rest) =>
          More(() => k(PendingOutput(arg).pos(tok.r), rest))
        })

      case (tok @ LogoWord("to")) :: tail =>
        // Define a user procedure
        tail match
          case LogoWord(procName) :: rest =>
            val (requiredParams, optionalParams, restParam, bodyStart) = collectParams(rest)
            val (body, afterEnd) = collectUntilEnd(bodyStart)
            procedures(procName.toLowerCase) =
              UserProcedure(procName.toLowerCase, requiredParams, optionalParams, restParam, body)
            More(() => k(LogoNull().pos(tok.r), afterEnd))
          case _ => tok.r.error("expected procedure name after 'to'")

      // Unary minus - must be before generic LogoWord case
      case (tok @ LogoWord("-")) :: tail =>
        evalPrimaryCPS(tail, { (operand0, rest) =>
          resolveThenContinue(operand0, { operand =>
            val result = logoNumber(QuaternionDAL.negate(number(operand)))
            More(() => k(result.pos(tok.r), rest))
          })
        })

      case (tok @ LogoWord(s)) :: tail =>
        if s.head == '"' then
          More(() => k(LogoWord(s.tail).pos(tok.r), tail))
        else if s.head == ':' then
          val name = s.tail.toLowerCase
          vars.get(name) match
            case Some(v) => More(() => k(v, tail))
            case None    => tok.r.error(s"unknown variable '$name'")
        else if s.head.isDigit || (s.head == '-' && s != "-") then
          More(() => k(logoNumber(s, tok.r), tail))
        else
          lookup(s) match
            case None =>
              tok.r.error(s"unknown procedure, variable, or constant '$s'")

            case Some(BuiltinFunction0(_, func)) =>
              More(() => k(logoNumber(func()).pos(tok.r), tail))

            case Some(BuiltinFunction1(name, func)) =>
              evalargsCPS(name, 1, tail, Seq.empty, { (args, rest) =>
                val n = number(args.head)
                More(() => k(logoNumber(func(n)).pos(tok.r), rest))
              })

            case Some(BuiltinFunction2(name, func)) =>
              evalargsCPS(name, 2, tail, Seq.empty, { (args, rest) =>
                val Seq(a, b) = args.map(number)
                More(() => k(logoNumber(func(a, b)).pos(tok.r), rest))
              })

            case Some(BuiltinProcedure(name, argc, func)) =>
              evalargsCPS(name, argc, tail, Seq.empty, { (vals, rest) =>
                val res = func(this, vals) match
                  case v: LogoValue => v
                  case n: Number    => logoNumber(n)
                  case b: Boolean   => LogoBoolean(b)
                  case ()           => LogoUnit
                More(() => k(res.pos(tok.r), rest))
              })

            case Some(BuiltinVariadic(name, defaultArgs, _, func)) =>
              evalargsCPS(name, defaultArgs, tail, Seq.empty, { (vals, rest) =>
                val res = func(this, vals) match
                  case v: LogoValue => v
                  case n: Number    => logoNumber(n)
                  case b: Boolean   => LogoBoolean(b)
                  case ()           => LogoUnit
                More(() => k(res.pos(tok.r), rest))
              })

            case Some(up @ UserProcedure(name, reqParams, optParams, restParam, body)) =>
              evalargsCPS(name, reqParams.length, tail, Seq.empty, { (vals, rest) =>
                More(() => k(PendingCallMarker(up, vals).pos(tok.r), rest))
              })

            case Some(v: LogoValue) =>
              More(() => k(v, tail))

            case Some(p: Procedure) =>
              problem(tok.r, s"procedure of unknown type: '${p.name}'")

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
