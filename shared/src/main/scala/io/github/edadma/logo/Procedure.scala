package io.github.edadma.logo

import scala.language.postfixOps
import scala.math.{E, Pi, random}

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
    BuiltinProcedure(
      "print",
      1,
      {
        case (_, Seq(arg)) => println(arg)
      },
    ),
    BuiltinFunction2("sum", _ + _),
    BuiltinProcedure(
      "difference",
      2,
      {
        case (_, Seq(left, right)) => number(left) - number(right)
      },
    ),
    BuiltinProcedure(
      "product",
      2,
      {
        case (_, Seq(left, right)) => number(left) * number(right)
      },
    ),
    BuiltinProcedure(
      "quotient",
      2,
      {
        case (_, Seq(left, right)) => number(left) / number(right)
      },
    ),
    BuiltinProcedure(
      "remainder",
      2,
      {
        case (_, Seq(left, right)) => number(left) % number(right)
      },
    ),
    BuiltinFunction1("negate", -_),
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

          ctx.x = newx
          ctx.y = newy
      },
    ),
    BuiltinProcedure(
      "penup",
      0,
      {
        case (ctx, _) =>
          ctx.pen = false
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "pendown",
      0,
      {
        case (ctx, _) =>
          ctx.pen = true
          ctx.event()
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

          for _ <- 1 to times do
            ctx.interp(body)
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
        case (ctx, Seq(name, value)) => ctx.vars(name.toString) = value
      },
    ),
  ) map (p => p.name -> p) toMap

val synonyms =
  List(
    "+"            -> "sum",
    "-"            -> "difference",
    "*"            -> "product",
    "/"            -> "quotient",
    "%"            -> "remainder",
    "="            -> "equalp",
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
  ) map ((s, p) => s -> builtin(p)) toMap
