package io.github.edadma.logo

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom
import org.scalajs.dom.html

import scala.math.Pi

/** Animated Logo interpreter with async execution support for step-by-step animation */
@JSExportTopLevel("LogoAnimated")
class LogoAnimated(canvas: html.Canvas) extends js.Object:
  private val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  // Settings
  private var usePathRendering: Boolean = true
  private var backgroundColor: String = "white"
  private var foregroundColor: (Int, Int, Int) = (0, 0, 0)
  private var isDarkMode: Boolean = false

  // Animation state
  private var speed: Int = 0 // ms delay between yield points (0 = instant)
  private var paused: Boolean = false
  private var stopped: Boolean = false
  private var running: Boolean = false
  private var pendingTimeout: Int = -1

  // Current execution state
  private var currentState: EvalResult = Done(LogoNull())

  // Callbacks
  private var onStepCb: js.UndefOr[js.Function0[Unit]] = js.undefined
  private var onCompleteCb: js.UndefOr[js.Function1[js.UndefOr[String], Unit]] = js.undefined
  private var onErrorCb: js.UndefOr[js.Function1[String, Unit]] = js.undefined

  // The Logo interpreter
  private val logo = new Logo:
    def event(): Unit =
      if speed > 0 then render()

  // === Public API ===

  /** Set the animation speed in milliseconds between yield points. 0 = instant (no animation). */
  def setSpeed(ms: Int): Unit = speed = math.max(0, ms)

  /** Get the current animation speed */
  def getSpeed(): Int = speed

  /** Check if execution is paused */
  def isPaused(): Boolean = paused

  /** Check if execution is running */
  def isRunning(): Boolean = running

  /** Pause execution */
  def pause(): Unit = paused = true

  /** Resume execution after pause */
  def resume(): Unit =
    if paused && running then
      paused = false
      continueExecution()

  /** Stop execution completely */
  def stop(): Unit =
    stopped = true
    paused = false
    running = false
    if pendingTimeout >= 0 then
      dom.window.clearTimeout(pendingTimeout)
      pendingTimeout = -1

  /** Run a Logo program with animation */
  def run(program: String): Unit =
    stop() // Stop any existing execution
    stopped = false
    paused = false
    running = true

    if speed == 0 then
      // Instant mode - run synchronously
      try
        val result = logo.interp(program)
        render()
        running = false
        onCompleteCb.foreach { cb =>
          result match
            case LogoUnit => cb(js.undefined)
            case v        => cb(v.toString)
        }
      catch
        case e: Throwable =>
          running = false
          onErrorCb.foreach(_(e.getMessage))
    else
      // Animated mode - run asynchronously
      try
        currentState = logo.interpStart(program)
        continueExecution()
      catch
        case e: Throwable =>
          running = false
          onErrorCb.foreach(_(e.getMessage))

  /** Execute a single command synchronously (always instant, for REPL-style use) */
  def execute(command: String): js.UndefOr[String] =
    try
      val result = logo.interp(command)
      render()
      result match
        case LogoUnit => js.undefined
        case v        => v.toString
    catch
      case e: Throwable =>
        onErrorCb.foreach(_(e.getMessage))
        js.undefined

  /** Clear the screen and reset turtle */
  def clear(): Unit =
    stop()
    logo.clearscreen()
    render()

  // === Callbacks ===

  /** Set callback for each animation step (called after each yield point) */
  def onStep(cb: js.Function0[Unit]): Unit = onStepCb = cb

  /** Set callback for completion (receives result or undefined) */
  def onComplete(cb: js.Function1[js.UndefOr[String], Unit]): Unit = onCompleteCb = cb

  /** Set callback for errors */
  def onError(cb: js.Function1[String, Unit]): Unit = onErrorCb = cb

  /** Set output handler for print statements */
  def setOutputHandler(handler: js.Function1[String, Unit]): Unit =
    logo.setOutputHandler(s => handler(s))

  /** Clear the output handler */
  def clearOutputHandler(): Unit =
    logo.clearOutputHandler()

  // === Settings ===

  /** Set whether to use path-based rendering (smoother) or line-based */
  def setPathRendering(enabled: Boolean): Unit =
    usePathRendering = enabled
    render()

  /** Set the canvas background color */
  def setBackgroundColor(color: String): Unit =
    backgroundColor = color
    val (r, g, b) = parseColor(color)
    val luminance = (0.299 * r + 0.587 * g + 0.114 * b) / 255
    isDarkMode = luminance < 0.5
    render()

  /** Set the default pen color (for theme-aware drawing) */
  def setForegroundColor(color: String): Unit =
    val rgb = parseColor(color)
    foregroundColor = rgb
    logo.setDefaultColor(rgb)
    render()

  /** Set a global variable */
  def setVariable(name: String, value: Any): Unit = logo.setVariable(name, value)

  /** Get a global variable */
  def getVariable(name: String): js.UndefOr[String] =
    logo.getVariable(name) match
      case Some(v) => v.toString
      case None    => js.undefined

  // === Internal Execution ===

  private def continueExecution(): Unit =
    if stopped then
      running = false
      return

    if paused then
      // Check again later
      pendingTimeout = dom.window.setTimeout(() => continueExecution(), 50)
      return

    try
      // Run steps until yield point (draw/print) or completion
      var steps = 0
      val maxSteps = 5000

      while steps < maxSteps && !stopped && !paused do
        currentState match
          case Done(v) =>
            running = false
            render()
            onCompleteCb.foreach { cb =>
              v match
                case LogoUnit   => cb(js.undefined)
                case LogoNull() => cb(js.undefined)
                case other      => cb(other.toString)
            }
            return
          case _ =>
            currentState = logo.trampolineStep(currentState)
            steps += 1

            // Check if we should yield (drawing OR output command)
            if logo.shouldYield() then
              render()
              onStepCb.foreach(_())
              pendingTimeout = dom.window.setTimeout(() => continueExecution(), speed)
              return

      // Yield to prevent blocking even without visible output
      if !stopped then
        pendingTimeout = dom.window.setTimeout(() => continueExecution(), 0)

    catch
      case e: Throwable =>
        running = false
        onErrorCb.foreach(_(e.getMessage))

  // === Rendering ===

  /** Force a render */
  def render(): Unit =
    val width = canvas.width
    val height = canvas.height

    ctx.fillStyle = backgroundColor
    ctx.fillRect(0, 0, width, height)

    ctx.save()
    ctx.translate(width / 2.0, height / 2.0)
    ctx.scale(1, -1)

    if usePathRendering then renderWithPaths()
    else renderWithLines()

    logo.turtle match
      case Some((x, y, heading)) => drawTurtle(x, y, heading)
      case None                  =>

    ctx.restore()

  private def parseColor(color: String): (Int, Int, Int) =
    ctx.fillStyle = color
    val parsed = ctx.fillStyle.asInstanceOf[String]
    if parsed.startsWith("#") then
      val hex = parsed.drop(1)
      if hex.length == 6 then
        val r = Integer.parseInt(hex.substring(0, 2), 16)
        val g = Integer.parseInt(hex.substring(2, 4), 16)
        val b = Integer.parseInt(hex.substring(4, 6), 16)
        (r, g, b)
      else (0, 0, 0)
    else (0, 0, 0)

  private def renderWithPaths(): Unit =
    case class Style(color: (Int, Int, Int), width: Double)

    var currentColor: (Int, Int, Int) = foregroundColor
    var currentWidth: Double = 1.0
    var currentStyle: Option[Style] = None
    var pathStarted = false
    var lastX: Double = 0
    var lastY: Double = 0

    def flushPath(): Unit =
      if pathStarted && currentStyle.isDefined then
        val Style((r, g, b), width) = currentStyle.get
        ctx.strokeStyle = s"rgb($r,$g,$b)"
        ctx.lineWidth = width
        ctx.lineCap = "round"
        ctx.lineJoin = "round"
        ctx.stroke()
        pathStarted = false
        currentStyle = None

    logo.drawing.foreach {
      case DrawSetColor(colorOpt) =>
        currentColor = colorOpt.getOrElse(foregroundColor)

      case DrawSetWidth(width) =>
        currentWidth = width

      case DrawLine(x1, y1, x2, y2) =>
        val style = Style(currentColor, currentWidth)

        if !currentStyle.contains(style) then
          flushPath()
          currentStyle = Some(style)
          ctx.beginPath()
          ctx.moveTo(x1, y1)
          pathStarted = true
          lastX = x1
          lastY = y1

        if !pathStarted || lastX != x1 || lastY != y1 then
          if !pathStarted then
            ctx.beginPath()
            pathStarted = true
          ctx.moveTo(x1, y1)

        ctx.lineTo(x2, y2)
        lastX = x2
        lastY = y2

      case DrawArc(x, y, heading, angleDeg, radius) =>
        flushPath()
        renderArc(x, y, heading, angleDeg, radius, currentColor, currentWidth)

      case DrawLabel(x, y, heading, text) =>
        flushPath()
        renderLabel(x, y, heading, text)
    }

    flushPath()

  private def renderWithLines(): Unit =
    var currentColor: (Int, Int, Int) = foregroundColor
    var currentWidth: Double = 1.0

    logo.drawing.foreach {
      case DrawSetColor(colorOpt) =>
        currentColor = colorOpt.getOrElse(foregroundColor)

      case DrawSetWidth(width) =>
        currentWidth = width

      case DrawLine(x1, y1, x2, y2) =>
        val (r, g, b) = currentColor
        ctx.strokeStyle = s"rgb($r,$g,$b)"
        ctx.lineWidth = currentWidth
        ctx.beginPath()
        ctx.moveTo(x1, y1)
        ctx.lineTo(x2, y2)
        ctx.stroke()

      case DrawArc(x, y, heading, angleDeg, radius) =>
        renderArc(x, y, heading, angleDeg, radius, currentColor, currentWidth)

      case DrawLabel(x, y, heading, text) =>
        renderLabel(x, y, heading, text)
    }

  private def renderLabel(x: Double, y: Double, heading: Double, text: String): Unit =
    ctx.save()
    ctx.translate(x, y)
    ctx.rotate(heading)
    ctx.scale(1, -1)
    ctx.fillStyle = "black"
    ctx.font = "20px sans-serif"
    ctx.fillText(text, 0, 0)
    ctx.restore()

  private def renderArc(x: Double, y: Double, heading: Double, angleDeg: Double, radius: Double, color: (Int, Int, Int), width: Double): Unit =
    val (r, g, b) = color
    val sign = if angleDeg >= 0 then 1.0 else -1.0
    val absAngle = math.abs(angleDeg)

    val perpAngle = heading + sign * Pi / 2
    val cx = x + radius * math.cos(perpAngle)
    val cy = y + radius * math.sin(perpAngle)

    val startAngle = math.atan2(y - cy, x - cx)

    val sweepRad = math.toRadians(absAngle)
    val endAngle = if angleDeg >= 0 then startAngle - sweepRad else startAngle + sweepRad

    ctx.strokeStyle = s"rgb($r,$g,$b)"
    ctx.lineWidth = width
    ctx.lineCap = "round"
    ctx.beginPath()
    ctx.arc(cx, cy, radius, startAngle, endAngle, angleDeg >= 0)
    ctx.stroke()

  private def drawTurtle(x: Double, y: Double, heading: Double): Unit =
    ctx.save()
    ctx.translate(x, y)
    ctx.rotate(heading + Pi / 2)

    val (shellFill, shellOutline, shellPattern, skinColor, skinOutline) =
      if isDarkMode then
        ("#2d5a27", "#1a3a18", "#3d7a37", "#4a7a44", "#1a3a18")
      else
        ("#5a9a50", "#3a6a38", "#7aba70", "#7ab070", "#3a6a38")

    // Tail
    ctx.beginPath()
    ctx.moveTo(0, 10)
    ctx.lineTo(0, 14)
    ctx.strokeStyle = skinColor
    ctx.lineWidth = 2
    ctx.lineCap = "round"
    ctx.stroke()

    // Legs
    ctx.fillStyle = skinColor
    ctx.strokeStyle = skinOutline
    ctx.lineWidth = 1
    ctx.beginPath()
    ctx.ellipse(-7, -6, 3, 5, 0.4, 0, 2 * Pi)
    ctx.fill()
    ctx.stroke()
    ctx.beginPath()
    ctx.ellipse(7, -6, 3, 5, -0.4, 0, 2 * Pi)
    ctx.fill()
    ctx.stroke()
    ctx.beginPath()
    ctx.ellipse(-6, 6, 3, 4, 0.3, 0, 2 * Pi)
    ctx.fill()
    ctx.stroke()
    ctx.beginPath()
    ctx.ellipse(6, 6, 3, 4, -0.3, 0, 2 * Pi)
    ctx.fill()
    ctx.stroke()

    // Head
    ctx.beginPath()
    ctx.ellipse(0, -14, 4, 5, 0, 0, 2 * Pi)
    ctx.fillStyle = skinColor
    ctx.fill()
    ctx.strokeStyle = skinOutline
    ctx.lineWidth = 1.5
    ctx.stroke()

    // Eyes
    ctx.fillStyle = "black"
    ctx.beginPath()
    ctx.arc(-1.5, -15, 1, 0, 2 * Pi)
    ctx.arc(1.5, -15, 1, 0, 2 * Pi)
    ctx.fill()

    // Shell
    ctx.beginPath()
    ctx.ellipse(0, 0, 8, 10, 0, 0, 2 * Pi)
    ctx.fillStyle = shellFill
    ctx.fill()
    ctx.strokeStyle = shellOutline
    ctx.lineWidth = 1.5
    ctx.stroke()

    // Shell pattern
    ctx.strokeStyle = shellPattern
    ctx.lineWidth = 1
    ctx.beginPath()
    ctx.ellipse(0, 0, 5, 6, 0, 0, 2 * Pi)
    ctx.stroke()

    ctx.restore()
