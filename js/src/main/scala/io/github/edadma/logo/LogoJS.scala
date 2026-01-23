package io.github.edadma.logo

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom
import org.scalajs.dom.html

import scala.math.Pi

/** Singleton representing "no value" from commands like print, fd, etc. */
@JSExportTopLevel("LogoUnit")
object LogoUnitJS extends js.Object

@js.native
trait LogoDrawing extends js.Object:
  val lines: js.Array[LineData] = js.native
  val labels: js.Array[LabelData] = js.native
  val arcs: js.Array[ArcData] = js.native

@js.native
trait ArcData extends js.Object:
  val x: Double = js.native
  val y: Double = js.native
  val heading: Double = js.native
  val angle: Double = js.native
  val radius: Double = js.native
  val color: String = js.native
  val width: Double = js.native

@js.native
trait LineData extends js.Object:
  val x1: Double = js.native
  val y1: Double = js.native
  val x2: Double = js.native
  val y2: Double = js.native
  val color: String = js.native
  val width: Double = js.native

@js.native
trait LabelData extends js.Object:
  val x: Double = js.native
  val y: Double = js.native
  val heading: Double = js.native
  val text: String = js.native

@js.native
trait TurtleState extends js.Object:
  val x: Double = js.native
  val y: Double = js.native
  val heading: Double = js.native
  val visible: Boolean = js.native

@JSExportTopLevel("Logo")
class LogoJS(canvas: html.Canvas) extends js.Object:
  private val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  private var usePathRendering: Boolean = true
  private var autoRender: Boolean = true
  private var initialized: Boolean = false
  private var backgroundColor: String = "white"
  private var eventHandler: Option[js.Function0[Unit]] = None

  private val logo = new Logo:
    def event(): Unit =
      if initialized then
        eventHandler match
          case Some(handler) => handler()
          case None => if autoRender then render()

  initialized = true

  /** Run a Logo program */
  def run(program: String): Unit =
    logo.interp(program)
    if !autoRender then render()

  /** Execute a single command, returns result (undefined for commands with no output) */
  def execute(command: String): js.UndefOr[String] =
    val result = logo.interp(command)
    if !autoRender then render()
    result match
      case LogoUnit => js.undefined
      case v        => v.toString

  /** Clear the screen and reset turtle */
  def clear(): Unit =
    logo.clearscreen()
    render()

  /** Set whether to use path-based rendering (smoother) or line-based */
  def setPathRendering(enabled: Boolean): Unit =
    usePathRendering = enabled
    render()

  /** Set whether to auto-render after each command */
  def setAutoRender(enabled: Boolean): Unit =
    autoRender = enabled

  /** Set the canvas background color */
  def setBackgroundColor(color: String): Unit =
    backgroundColor = color
    render()

  /** Set the default pen color (used after clear) */
  def setForegroundColor(color: String): Unit =
    val rgb = parseColor(color)
    logo.setDefaultColor(rgb)
    render()

  /** Parse a CSS color string to RGB tuple */
  private def parseColor(color: String): (Int, Int, Int) =
    // Use canvas to parse any CSS color
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

  /** Set a callback for print output (instead of console) */
  def setOutputHandler(handler: js.Function1[String, Unit]): Unit =
    logo.setOutputHandler(s => handler(s))

  /** Clear the output handler (print goes to console) */
  def clearOutputHandler(): Unit =
    logo.clearOutputHandler()

  /** Set a callback for turtle events (called after each drawing command) */
  def setEventHandler(handler: js.Function0[Unit]): Unit =
    eventHandler = Some(handler)

  /** Clear the event handler (use default auto-render behavior) */
  def clearEventHandler(): Unit =
    eventHandler = None

  /** Set a global variable */
  def setVariable(name: String, value: Any): Unit = logo.setVariable(name, value)

  /** Get a global variable */
  def getVariable(name: String): Option[LogoValue] = logo.getVariable(name)

  /** Force a render */
  def render(): Unit =
    val width = canvas.width
    val height = canvas.height

    // Clear canvas
    ctx.fillStyle = backgroundColor
    ctx.fillRect(0, 0, width, height)

    // Set up coordinate system (origin at center, y-up)
    ctx.save()
    ctx.translate(width / 2.0, height / 2.0)
    ctx.scale(1, -1)

    if usePathRendering then renderWithPaths()
    else renderWithLines()

    // Draw turtle
    logo.turtle match
      case Some((x, y, heading)) => drawTurtle(x, y, heading)
      case None                  =>

    ctx.restore()

  /** Get the current drawing as data (for custom rendering) */
  def getDrawing(): LogoDrawing =
    val lines = js.Array[LineData]()
    val labels = js.Array[LabelData]()
    val arcs = js.Array[ArcData]()

    logo.drawing.foreach {
      case DrawLine(x1, y1, x2, y2, (r, g, b), width) =>
        lines.push(js.Dynamic.literal(
          x1 = x1, y1 = y1, x2 = x2, y2 = y2,
          color = s"rgb($r,$g,$b)", width = width
        ).asInstanceOf[LineData])
      case DrawLabel(x, y, heading, text) =>
        labels.push(js.Dynamic.literal(
          x = x, y = y, heading = heading, text = text
        ).asInstanceOf[LabelData])
      case DrawArc(x, y, heading, angle, radius, (r, g, b), width) =>
        arcs.push(js.Dynamic.literal(
          x = x, y = y, heading = heading, angle = angle, radius = radius,
          color = s"rgb($r,$g,$b)", width = width
        ).asInstanceOf[ArcData])
    }

    js.Dynamic.literal(lines = lines, labels = labels, arcs = arcs).asInstanceOf[LogoDrawing]

  /** Get the current turtle state */
  def getTurtle(): TurtleState =
    logo.turtle match
      case Some((x, y, heading)) =>
        js.Dynamic.literal(x = x, y = y, heading = heading, visible = true).asInstanceOf[TurtleState]
      case None =>
        js.Dynamic.literal(x = 0, y = 0, heading = Pi / 2, visible = false).asInstanceOf[TurtleState]

  private def renderWithPaths(): Unit =
    case class Style(color: (Int, Int, Int), width: Double)

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
      case DrawLine(x1, y1, x2, y2, color, width) =>
        val style = Style(color, width)

        if !currentStyle.contains(style) then
          flushPath()
          currentStyle = Some(style)
          ctx.beginPath()
          ctx.moveTo(x1, y1)
          pathStarted = true
          lastX = x1
          lastY = y1

        // Check for discontinuity
        if !pathStarted || lastX != x1 || lastY != y1 then
          if !pathStarted then
            ctx.beginPath()
            pathStarted = true
          ctx.moveTo(x1, y1)

        ctx.lineTo(x2, y2)
        lastX = x2
        lastY = y2

      case DrawArc(x, y, heading, angleDeg, radius, color, width) =>
        flushPath()
        renderArc(x, y, heading, angleDeg, radius, color, width)

      case DrawLabel(x, y, heading, text) =>
        flushPath()
        renderLabel(x, y, heading, text)
    }

    flushPath()

  private def renderWithLines(): Unit =
    logo.drawing.foreach {
      case DrawLine(x1, y1, x2, y2, (r, g, b), width) =>
        ctx.strokeStyle = s"rgb($r,$g,$b)"
        ctx.lineWidth = width
        ctx.beginPath()
        ctx.moveTo(x1, y1)
        ctx.lineTo(x2, y2)
        ctx.stroke()

      case DrawArc(x, y, heading, angleDeg, radius, (r, g, b), width) =>
        renderArc(x, y, heading, angleDeg, radius, (r, g, b), width)

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
    // For positive angle: center is to the left of turtle (perpendicular)
    // For negative angle: center is to the right
    val sign = if angleDeg >= 0 then 1.0 else -1.0
    val absAngle = math.abs(angleDeg)

    // Center is perpendicular to turtle's heading
    // heading is in radians (internal: 0=east, CCW positive)
    // Perpendicular to the left: heading + π/2
    val perpAngle = heading + sign * Pi / 2
    val cx = x + radius * math.cos(perpAngle)
    val cy = y + radius * math.sin(perpAngle)

    // Start angle: from center to turtle position
    val startAngle = math.atan2(y - cy, x - cx)

    // End angle: sweep by angleDeg (converted to radians)
    // Positive angleDeg = CCW on circle (which is forward/right from turtle's view)
    val sweepRad = math.toRadians(absAngle)
    val endAngle = if angleDeg >= 0 then startAngle - sweepRad else startAngle + sweepRad

    ctx.strokeStyle = s"rgb($r,$g,$b)"
    ctx.lineWidth = width
    ctx.lineCap = "round"
    ctx.beginPath()
    // counterclockwise parameter: true for positive angle (sweep is subtracted)
    ctx.arc(cx, cy, radius, startAngle, endAngle, angleDeg >= 0)
    ctx.stroke()

  private def drawTurtle(x: Double, y: Double, heading: Double): Unit =
    ctx.save()
    ctx.translate(x, y)
    ctx.rotate(heading + Pi / 2)

    // Tail (behind shell)
    ctx.beginPath()
    ctx.moveTo(0, 10)
    ctx.lineTo(0, 14)
    ctx.strokeStyle = "#4a7a44"
    ctx.lineWidth = 2
    ctx.lineCap = "round"
    ctx.stroke()

    // Legs (behind shell)
    ctx.fillStyle = "#4a7a44"
    ctx.strokeStyle = "#1a3a18"
    ctx.lineWidth = 1
    // Front legs
    ctx.beginPath()
    ctx.ellipse(-7, -6, 3, 5, 0.4, 0, 2 * Pi)
    ctx.fill()
    ctx.stroke()
    ctx.beginPath()
    ctx.ellipse(7, -6, 3, 5, -0.4, 0, 2 * Pi)
    ctx.fill()
    ctx.stroke()
    // Back legs
    ctx.beginPath()
    ctx.ellipse(-6, 6, 3, 4, 0.3, 0, 2 * Pi)
    ctx.fill()
    ctx.stroke()
    ctx.beginPath()
    ctx.ellipse(6, 6, 3, 4, -0.3, 0, 2 * Pi)
    ctx.fill()
    ctx.stroke()

    // Head (behind shell)
    ctx.beginPath()
    ctx.ellipse(0, -14, 4, 5, 0, 0, 2 * Pi)
    ctx.fillStyle = "#4a7a44"
    ctx.fill()
    ctx.strokeStyle = "#1a3a18"
    ctx.lineWidth = 1.5
    ctx.stroke()

    // Eyes
    ctx.fillStyle = "black"
    ctx.beginPath()
    ctx.arc(-1.5, -15, 1, 0, 2 * Pi)
    ctx.arc(1.5, -15, 1, 0, 2 * Pi)
    ctx.fill()

    // Shell (on top)
    ctx.beginPath()
    ctx.ellipse(0, 0, 8, 10, 0, 0, 2 * Pi)
    ctx.fillStyle = "#2d5a27"
    ctx.fill()
    ctx.strokeStyle = "#1a3a18"
    ctx.lineWidth = 1.5
    ctx.stroke()

    // Shell pattern
    ctx.strokeStyle = "#3d7a37"
    ctx.lineWidth = 1
    ctx.beginPath()
    ctx.ellipse(0, 0, 5, 6, 0, 0, 2 * Pi)
    ctx.stroke()

    ctx.restore()
