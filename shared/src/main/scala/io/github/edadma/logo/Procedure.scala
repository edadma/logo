package io.github.edadma.logo

import io.github.edadma.dal.QuaternionDAL
import io.github.edadma.numbers.QuaternionBigInt

import scala.language.postfixOps
import scala.math.{E, Pi}

abstract class Procedure:
  val name: String

case class BuiltinProcedure(name: String, args: Int, func: PartialFunction[(Logo, Seq[LogoValue]), Any])
    extends Procedure
case class BuiltinFunction0(name: String, func: () => Number)               extends Procedure
case class BuiltinFunction1(name: String, func: Number => Number)           extends Procedure
case class BuiltinFunction2(name: String, func: (Number, Number) => Number) extends Procedure
// Variadic: defaultArgs is used without parens, with parens accepts minArgs or more
case class BuiltinVariadic(name: String, defaultArgs: Int, minArgs: Int, func: (Logo, Seq[LogoValue]) => Any)
    extends Procedure
// User-defined procedure
case class UserProcedure(name: String, params: Seq[String], body: Seq[LogoValue]) extends Procedure

val builtin =
  List[Procedure](
    BuiltinFunction0("pi", () => Pi),
    BuiltinFunction0("e", () => E),
    BuiltinFunction0("i", () => QuaternionBigInt(0, 1, 0, 0)),
    BuiltinFunction0("j", () => QuaternionBigInt(0, 0, 1, 0)),
    BuiltinFunction0("k", () => QuaternionBigInt(0, 0, 0, 1)),
    BuiltinFunction1("random", limit => QuaternionDAL.compute("*", scala.math.random, limit)),
    BuiltinVariadic(
      "print",
      1,
      1,
      (_, args) => println(args.mkString(" ")),
    ),
    BuiltinVariadic(
      "list",
      2,
      0,
      (_, args) => LogoList(args, args :+ EOIToken()),
    ),
    BuiltinVariadic(
      "word",
      2,
      0,
      (_, args) => LogoWord(args.map(_.toString).mkString),
    ),
    BuiltinVariadic(
      "sentence",
      2,
      0,
      (_, args) =>
        val flat = args.flatMap {
          case LogoList(elems, _) => elems
          case v                  => Seq(v)
        }
        LogoList(flat, flat :+ EOIToken()),
    ),
    BuiltinVariadic(
      "sum",
      2,
      0,
      (_, args) => args.map(number).reduceOption(QuaternionDAL.compute("+", _, _)).getOrElse(0),
    ),
    BuiltinFunction2("difference", QuaternionDAL.compute("-", _, _)),
    BuiltinVariadic(
      "product",
      2,
      0,
      (_, args) => args.map(number).reduceOption(QuaternionDAL.compute("*", _, _)).getOrElse(1),
    ),
    BuiltinFunction2("quotient", QuaternionDAL.compute("/", _, _)),
    BuiltinFunction2("remainder", QuaternionDAL.compute("mod", _, _)),
    BuiltinFunction2("pow", QuaternionDAL.compute("^", _, _)),
    BuiltinFunction1("negate", QuaternionDAL.negate),
    BuiltinFunction1("sin", QuaternionDAL.sinFunction),
    BuiltinFunction1("cos", QuaternionDAL.cosFunction),
    BuiltinFunction1("tan", QuaternionDAL.tanFunction),
    BuiltinFunction1("sinh", QuaternionDAL.sinhFunction),
    BuiltinFunction1("cosh", QuaternionDAL.coshFunction),
    BuiltinFunction1("tanh", QuaternionDAL.tanhFunction),
    BuiltinFunction1("sqrt", QuaternionDAL.sqrtFunction),
    BuiltinFunction1("exp", QuaternionDAL.expFunction),
    BuiltinFunction1("ln", QuaternionDAL.lnFunction),
    BuiltinProcedure(
      "equalp",
      2,
      {
        case (_, Seq(left, right)) => left == right
      },
    ),
    BuiltinProcedure(
      "notequalp",
      2,
      {
        case (_, Seq(left, right)) => left != right
      },
    ),
    BuiltinProcedure(
      "lessp",
      2,
      {
        case (_, Seq(left, right)) => QuaternionDAL.relate("<", number(left), number(right))
      },
    ),
    BuiltinProcedure(
      "greaterp",
      2,
      {
        case (_, Seq(left, right)) => QuaternionDAL.relate(">", number(left), number(right))
      },
    ),
    BuiltinProcedure(
      "lessequalp",
      2,
      {
        case (_, Seq(left, right)) => QuaternionDAL.relate("<=", number(left), number(right))
      },
    ),
    BuiltinProcedure(
      "greaterequalp",
      2,
      {
        case (_, Seq(left, right)) => QuaternionDAL.relate(">=", number(left), number(right))
      },
    ),
    BuiltinProcedure(
      "and",
      2,
      {
        case (_, Seq(left, right)) => boolean(left) && boolean(right)
      },
    ),
    BuiltinProcedure(
      "or",
      2,
      {
        case (_, Seq(left, right)) => boolean(left) || boolean(right)
      },
    ),
    BuiltinProcedure(
      "not",
      1,
      {
        case (_, Seq(arg)) => !boolean(arg)
      },
    ),
    BuiltinProcedure(
      "forward",
      1,
      {
        case (ctx, Seq(distance)) =>
          val (x2, y2) = ctx.computeEndpoint(number(distance).doubleValue)

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
          ctx.heading = ctx.computeTurn(number(turn).doubleValue)
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "back",
      1,
      {
        case (ctx, Seq(distance)) =>
          val (x2, y2) = ctx.computeEndpoint(-number(distance).doubleValue)

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
          ctx.heading = ctx.computeTurn(-number(turn).doubleValue)
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
          ctx.width = number(width).doubleValue
          ctx.event()
        case (ctx, Seq(width)) =>
          ctx.width = number(width).doubleValue
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "setcolor",
      1,
      {
        case (ctx, Seq(LogoList(Seq(r, g, b), _))) =>
          ctx.color = (number(r).intValue, number(g).intValue, number(b).intValue)
          ctx.event()
        case (ctx, Seq(LogoNumber(n))) =>
          ctx.color = colorArray(n.intValue)
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
          val newx = number(x).doubleValue
          val newy = number(y).doubleValue

          if ctx.pen then ctx.draws += DrawLine(ctx.x, ctx.y, newx, newy, ctx.color, ctx.width)
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
          val times = number(left).intValue
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

          if cond then ctx.interp(body) else LogoNull()
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
    BuiltinProcedure(
      "run",
      1,
      {
        case (ctx, Seq(code)) =>
          // Convert list to string and re-parse to enable operator splitting
          ctx.interp(code.toString)
      },
    ),
    BuiltinProcedure(
      "output",
      1,
      {
        case (_, Seq(value)) => throw OutputException(value)
      },
    ),
    BuiltinProcedure(
      "stop",
      0,
      {
        case _ => throw StopException()
      },
    ),
  ) map (p => p.name -> p) toMap

val synonyms =
  List(
    "+"            -> "sum",
    "-"            -> "difference",
    "*"            -> "product",
    "/"            -> "quotient",
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
    "se"           -> "sentence",
    "pr"           -> "print",
    "op"           -> "output",
  ) map ((s, p) => s -> builtin(p)) toMap
