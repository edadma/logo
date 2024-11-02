package io.github.edadma.logo

import scala.language.postfixOps

case class Procedure(name: String, args: Int, func: PartialFunction[(Logo, Seq[LogoValue]), Any])

val procedures =
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
  ) map (p => p.name -> p) toMap
