package io.github.edadma.logo

import scala.language.postfixOps

case class Procedure(name: String, args: Int, func: PartialFunction[(Logo, Seq[LogoValue]), Any])

val builtin =
  List(
    Procedure(
      "print",
      1,
      {
        case (_, Seq(arg)) => println(arg)
      },
    ),
    Procedure(
      "sum",
      2,
      {
        case (_, Seq(left, right)) => number(left) + number(right)
      },
    ),
    Procedure(
      "difference",
      2,
      {
        case (_, Seq(left, right)) => number(left) - number(right)
      },
    ),
    Procedure(
      "product",
      2,
      {
        case (_, Seq(left, right)) => number(left) * number(right)
      },
    ),
    Procedure(
      "quotient",
      2,
      {
        case (_, Seq(left, right)) => number(left) / number(right)
      },
    ),
    Procedure(
      "remainder",
      2,
      {
        case (_, Seq(left, right)) => number(left) % number(right)
      },
    ),
    Procedure(
      "equalp",
      2,
      {
        case (_, Seq(left, right)) => left == right
      },
    ),
    Procedure(
      "forward",
      1,
      {
        case (ctx, Seq(distance)) =>
          val (x2, y2) = ctx.computeEndpoint(number(distance))

          if ctx.pen then ctx.draws += Line(ctx.x, ctx.y, x2, y2, ctx.color, ctx.width)
          ctx.x = x2
          ctx.y = y2
          ctx.event()
      },
    ),
    Procedure(
      "right",
      1,
      {
        case (ctx, Seq(turn)) =>
          ctx.heading = ctx.computeTurn(number(turn))
          ctx.event()
      },
    ),
    Procedure(
      "back",
      1,
      {
        case (ctx, Seq(distance)) =>
          val (x2, y2) = ctx.computeEndpoint(-number(distance))

          if ctx.pen then ctx.draws += Line(ctx.x, ctx.y, x2, y2, ctx.color, ctx.width)
          ctx.x = x2
          ctx.y = y2
          ctx.event()
      },
    ),
    Procedure(
      "left",
      1,
      {
        case (ctx, Seq(turn)) =>
          ctx.heading = ctx.computeTurn(-number(turn))
          ctx.event()
      },
    ),
    Procedure(
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
    Procedure(
      "clearscreen",
      0,
      {
        case (ctx, _) =>
          ctx.clearscreen()
          ctx.event()
      },
    ),
    Procedure(
      "home",
      0,
      {
        case (ctx, _) =>
          ctx.home()
          ctx.event()
      },
    ),
    Procedure(
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
    Procedure(
      "penup",
      0,
      {
        case (ctx, _) =>
          ctx.pen = false
          ctx.event()
      },
    ),
    Procedure(
      "pendown",
      0,
      {
        case (ctx, _) =>
          ctx.pen = true
          ctx.event()
      },
    ),
    Procedure(
      "hideturtle",
      0,
      {
        case (ctx, _) =>
          ctx.show = false
          ctx.event()
      },
    ),
    Procedure(
      "showturtle",
      0,
      {
        case (ctx, _) =>
          ctx.show = true
          ctx.event()
      },
    ),
    Procedure(
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
    Procedure(
      "if",
      2,
      {
        case (ctx, Seq(left, right)) =>
          val cond = boolean(left)
          val body = list(right)

          if cond then ctx.interp(body)
      },
    ),
    Procedure(
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
    Procedure(
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
    "repete"       -> "repeat",
    "si"           -> "if",
    "siou"         -> "ifelse",
    "rends"        -> "make",
  ) map ((s, p) => s -> builtin(p)) toMap
