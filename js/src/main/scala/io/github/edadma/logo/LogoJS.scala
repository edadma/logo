package io.github.edadma.logo

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom
import org.scalajs.dom.html

import scala.math.Pi

@js.native
trait LogoDrawing extends js.Object:
  val lines: js.Array[LineData] = js.native
  val labels: js.Array[LabelData] = js.native

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

  private val logo = new Logo:
    def event(): Unit = if initialized && autoRender then render()

  initialized = true

  /** Run a Logo program */
  def run(program: String): Unit =
    logo.interp(program)
    if !autoRender then render()

  /** Execute a single command */
  def execute(command: String): Unit =
    logo.interp(command)
    if !autoRender then render()

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

  /** Set a callback for print output (instead of console) */
  def setOutputHandler(handler: js.Function1[String, Unit]): Unit =
    logo.setOutputHandler(s => handler(s))

  /** Clear the output handler (print goes to console) */
  def clearOutputHandler(): Unit =
    logo.clearOutputHandler()

  /** Force a render */
  def render(): Unit =
    val width = canvas.width
    val height = canvas.height

    // Clear canvas
    ctx.fillStyle = "white"
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
    }

    js.Dynamic.literal(lines = lines, labels = labels).asInstanceOf[LogoDrawing]

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

  private def drawTurtle(x: Double, y: Double, heading: Double): Unit =
    val w = 15.0
    val h = 20.0

    ctx.save()
    ctx.translate(x, y)
    ctx.rotate(heading - Pi / 2)

    ctx.beginPath()
    ctx.moveTo(0, 0)
    ctx.lineTo(-w / 2, h / 2)
    ctx.lineTo(0, h)
    ctx.lineTo(w / 2, h / 2)
    ctx.closePath()

    ctx.strokeStyle = "green"
    ctx.lineWidth = 2
    ctx.stroke()

    ctx.restore()
