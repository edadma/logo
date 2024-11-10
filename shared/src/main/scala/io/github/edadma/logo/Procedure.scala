package io.github.edadma.logo

import java.awt.Color
import scala.language.postfixOps
import scala.math.{E, Pi, cos, cosh, exp, log, pow, random, sin, sinh, sqrt, tan, tanh}

abstract class Procedure:
  val name: String

case class BuiltinProcedure(name: String, args: Int, func: PartialFunction[(Logo, Seq[LogoValue]), Any])
    extends Procedure
case class BuiltinFunction0(name: String, func: () => Double)               extends Procedure
case class BuiltinFunction1(name: String, func: Double => Double)           extends Procedure
case class BuiltinFunction2(name: String, func: (Double, Double) => Double) extends Procedure

val builtin =
  List[Procedure](
    BuiltinFunction0("pi", () => Pi),
    BuiltinFunction0("e", () => E),
    BuiltinFunction1("random", limit => random * limit),
    BuiltinProcedure("print", 1, { case (_, Seq(arg)) => println(arg) }),
    BuiltinFunction2("sum", _ + _),
    BuiltinFunction2("difference", _ - _),
    BuiltinFunction2("product", _ * _),
    BuiltinFunction2("quotient", _ / _),
    BuiltinFunction2("remainder", _ % _),
    BuiltinFunction2("pow", pow),
    BuiltinFunction1("negate", -_),
    BuiltinFunction1("sin", sin),
    BuiltinFunction1("cos", cos),
    BuiltinFunction1("tan", tan),
    BuiltinFunction1("sinh", sinh),
    BuiltinFunction1("cosh", cosh),
    BuiltinFunction1("tanh", tanh),
    BuiltinFunction1("sqrt", sqrt),
    BuiltinFunction1("exp", exp),
    BuiltinFunction1("ln", log),
    BuiltinProcedure(
      "equalp",
      2,
      {
        case (_, Seq(left, right)) => left == right
      },
    ),
    BuiltinProcedure(
      "forward",
      1,
      {
        case (ctx, Seq(distance)) =>
          val (x2, y2) = ctx.computeEndpoint(number(distance))

          if ctx.pen then ctx.draws += DrawLine(ctx.x, ctx.y, x2, y2, ctx.color, ctx.width)
          ctx.x = x2
          ctx.y = y2
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "right",
      1,
      {
        case (ctx, Seq(turn)) =>
          ctx.heading = ctx.computeTurn(number(turn))
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "back",
      1,
      {
        case (ctx, Seq(distance)) =>
          val (x2, y2) = ctx.computeEndpoint(-number(distance))

          if ctx.pen then ctx.draws += DrawLine(ctx.x, ctx.y, x2, y2, ctx.color, ctx.width)
          ctx.x = x2
          ctx.y = y2
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "left",
      1,
      {
        case (ctx, Seq(turn)) =>
          ctx.heading = ctx.computeTurn(-number(turn))
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "label",
      1,
      {
        case (ctx, Seq(text)) =>
          ctx.draws += DrawLabel(ctx.x, ctx.y, ctx.heading, text.toString)
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "setpensize",
      1,
      {
        case (ctx, Seq(size @ LogoList(Seq(width, _), _))) =>
          ctx.width = number(width)
          ctx.event()
        case (ctx, Seq(width)) =>
          ctx.width = number(width)
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "setcolor",
      1,
      {
        case (ctx, Seq(LogoList(Seq(r, g, b), _))) =>
          ctx.color = (number(r).toInt, number(g).toInt, number(b).toInt)
          ctx.event()
        case (ctx, Seq(LogoNumber(_, d))) =>
          ctx.color = colorArray(d.toInt)
          ctx.event()
        case (ctx, Seq(LogoWord(c))) =>
          ctx.color = colorMap(c)
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "clearscreen",
      0,
      {
        case (ctx, _) =>
          ctx.clearscreen()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "home",
      0,
      {
        case (ctx, _) =>
          ctx.home()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "setxy",
      2,
      {
        case (ctx, Seq(x, y)) =>
          val newx = number(x)
          val newy = number(y)

          if ctx.pen then ctx.draws += DrawLine(ctx.x, ctx.y, newx, newy, ctx.color, ctx.width)
          ctx.x = newx
          ctx.y = newy
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "penup",
      0,
      {
        case (ctx, _) => ctx.pen = false
      },
    ),
    BuiltinProcedure(
      "pendown",
      0,
      {
        case (ctx, _) => ctx.pen = true
      },
    ),
    BuiltinProcedure(
      "hideturtle",
      0,
      {
        case (ctx, _) =>
          ctx.show = false
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "showturtle",
      0,
      {
        case (ctx, _) =>
          ctx.show = true
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "repeat",
      2,
      {
        case (ctx, Seq(left, right)) =>
          val times = number(left).toInt
          val body  = list(right)

          for _ <- 1 to times do ctx.interp(body)
      },
    ),
    BuiltinProcedure(
      "if",
      2,
      {
        case (ctx, Seq(left, right)) =>
          val cond = boolean(left)
          val body = list(right)

          if cond then ctx.interp(body)
      },
    ),
    BuiltinProcedure(
      "ifelse",
      3,
      {
        case (ctx, Seq(cond, yes, no)) =>
          val condv = boolean(cond)
          val yesv  = list(yes)
          val nov   = list(no)

          if condv then ctx.interp(yesv) else ctx.interp(nov)
      },
    ),
    BuiltinProcedure(
      "make",
      2,
      {
        case (ctx, Seq(name, value)) => ctx.vars(name.toString) = value.seal
      },
    ),
  ) map (p => p.name -> p) toMap

val synonyms =
  List(
    "dis"          -> "print",
    "fd"           -> "forward",
    "avance"       -> "forward",
    "av"           -> "forward",
    "cs"           -> "clearscreen",
    "effacerecran" -> "clearscreen",
    "ee"           -> "clearscreen",
    "pu"           -> "penup",
    "levecrayon"   -> "penup",
    "lc"           -> "penup",
    "pd"           -> "pendown",
    "baisscrayon"  -> "pendown",
    "bc"           -> "pendown",
    "ht"           -> "hideturtle",
    "cachetortue"  -> "hideturtle",
    "ct"           -> "hideturtle",
    "st"           -> "showturtle",
    "montretortue" -> "showturtle",
    "mt"           -> "showturtle",
    "rt"           -> "right",
    "droite"       -> "right",
    "dr"           -> "right",
    "bk"           -> "back",
    "recule"       -> "back",
    "re"           -> "back",
    "lt"           -> "left",
    "gauche"       -> "left",
    "gc"           -> "left",
    "taillecrayon" -> "setpensize",
    "origine"      -> "home",
    "placexy"      -> "setxy",
    "texte"        -> "label",
    "repete"       -> "repeat",
    "si"           -> "if",
    "siou"         -> "ifelse",
    "rends"        -> "make",
    "rnd"          -> "random",
    "aleatoire"    -> "random",
    "alt"          -> "random",
  ) map ((s, p) => s -> builtin(p)) toMap
