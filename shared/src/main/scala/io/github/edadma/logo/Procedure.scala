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
      "forward",
      1,
      {
        case (ctx, Seq(distance)) =>
          val (x2, y2) = ctx.computeEndpoint(number(distance))

          ctx.draws += Line(ctx.x, ctx.y, x2, y2, ctx.color)
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
  ) map (p => p.name -> p) toMap

val synonyms =
  List(
    "+"      -> "sum",
    "fd"     -> "forward",
    "avance" -> "forward",
    "av"     -> "forward",
    "rt"     -> "right",
    "droite" -> "right",
    "dr"     -> "right",
  ) map ((s, p) => s -> builtin(p)) toMap
