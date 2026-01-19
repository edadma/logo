package io.github.edadma.logo

import io.github.edadma.dal.QuaternionDAL
import io.github.edadma.numbers.{ComplexDouble, ComplexBigInt, ComplexRational, ComplexSmallRational, QuaternionBigInt, QuaternionDouble, QuaternionRational}

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
// User-defined procedure with optional and rest params
case class UserProcedure(
    name: String,
    requiredParams: Seq[String],
    optionalParams: Seq[(String, LogoValue)], // (name, default)
    restParam: Option[String],
    body: Seq[LogoValue],
) extends Procedure

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
      (ctx, args) => ctx.output(args.mkString(" ")),
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
    // List/word primitives
    BuiltinProcedure(
      "first",
      1,
      {
        case (_, Seq(LogoList(elems, _))) =>
          if elems.isEmpty then problem(null, "'first' requires non-empty list")
          else elems.head
        case (_, Seq(LogoWord(s))) =>
          if s.isEmpty then problem(null, "'first' requires non-empty word")
          else LogoWord(s.head.toString)
        case (_, Seq(other)) => problem(null, s"'first' requires a list or word, got $other")
      },
    ),
    BuiltinProcedure(
      "last",
      1,
      {
        case (_, Seq(LogoList(elems, _))) =>
          if elems.isEmpty then problem(null, "'last' requires non-empty list")
          else elems.last
        case (_, Seq(LogoWord(s))) =>
          if s.isEmpty then problem(null, "'last' requires non-empty word")
          else LogoWord(s.last.toString)
        case (_, Seq(other)) => problem(null, s"'last' requires a list or word, got $other")
      },
    ),
    BuiltinProcedure(
      "butfirst",
      1,
      {
        case (_, Seq(LogoList(elems, _))) =>
          if elems.isEmpty then problem(null, "'butfirst' requires non-empty list")
          else
            val rest = elems.tail
            LogoList(rest, rest :+ EOIToken())
        case (_, Seq(LogoWord(s))) =>
          if s.isEmpty then problem(null, "'butfirst' requires non-empty word")
          else LogoWord(s.tail)
        case (_, Seq(other)) => problem(null, s"'butfirst' requires a list or word, got $other")
      },
    ),
    BuiltinProcedure(
      "butlast",
      1,
      {
        case (_, Seq(LogoList(elems, _))) =>
          if elems.isEmpty then problem(null, "'butlast' requires non-empty list")
          else
            val init = elems.init
            LogoList(init, init :+ EOIToken())
        case (_, Seq(LogoWord(s))) =>
          if s.isEmpty then problem(null, "'butlast' requires non-empty word")
          else LogoWord(s.init)
        case (_, Seq(other)) => problem(null, s"'butlast' requires a list or word, got $other")
      },
    ),
    BuiltinProcedure(
      "fput",
      2,
      {
        case (_, Seq(elem, LogoList(elems, _))) =>
          val newList = elem +: elems
          LogoList(newList, newList :+ EOIToken())
        case (_, Seq(LogoWord(c), LogoWord(s))) if c.length == 1 =>
          LogoWord(c + s)
        case (_, Seq(_, other)) => problem(null, s"'fput' requires a list or word as second argument, got $other")
      },
    ),
    BuiltinProcedure(
      "lput",
      2,
      {
        case (_, Seq(elem, LogoList(elems, _))) =>
          val newList = elems :+ elem
          LogoList(newList, newList :+ EOIToken())
        case (_, Seq(LogoWord(c), LogoWord(s))) if c.length == 1 =>
          LogoWord(s + c)
        case (_, Seq(_, other)) => problem(null, s"'lput' requires a list or word as second argument, got $other")
      },
    ),
    BuiltinProcedure(
      "item",
      2,
      {
        case (_, Seq(idx, LogoList(elems, _))) =>
          val i = number(idx).intValue
          if i < 1 || i > elems.length then problem(null, s"'item' index $i out of range 1..${elems.length}")
          else elems(i - 1) // Logo uses 1-based indexing
        case (_, Seq(idx, LogoWord(s))) =>
          val i = number(idx).intValue
          if i < 1 || i > s.length then problem(null, s"'item' index $i out of range 1..${s.length}")
          else LogoWord(s(i - 1).toString)
        case (_, Seq(_, other)) => problem(null, s"'item' requires a list or word as second argument, got $other")
      },
    ),
    BuiltinProcedure(
      "count",
      1,
      {
        case (_, Seq(LogoList(elems, _))) => elems.length
        case (_, Seq(LogoWord(s)))        => s.length
        case (_, Seq(other))              => problem(null, s"'count' requires a list or word, got $other")
      },
    ),
    BuiltinProcedure(
      "emptyp",
      1,
      {
        case (_, Seq(LogoList(elems, _))) => elems.isEmpty
        case (_, Seq(LogoWord(s)))        => s.isEmpty
        case (_, Seq(other))              => problem(null, s"'emptyp' requires a list or word, got $other")
      },
    ),
    BuiltinProcedure(
      "listp",
      1,
      {
        case (_, Seq(_: LogoList)) => true
        case (_, Seq(_))           => false
      },
    ),
    BuiltinProcedure(
      "wordp",
      1,
      {
        case (_, Seq(_: LogoWord)) => true
        case (_, Seq(_))           => false
      },
    ),
    BuiltinProcedure(
      "numberp",
      1,
      {
        case (_, Seq(_: LogoNumber)) => true
        case (_, Seq(_))             => false
      },
    ),
    BuiltinProcedure(
      "memberp",
      2,
      {
        case (_, Seq(elem, LogoList(elems, _))) => elems.contains(elem)
        case (_, Seq(LogoWord(c), LogoWord(s))) => s.contains(c)
        case (_, Seq(_, other)) => problem(null, s"'memberp' requires a list or word as second argument, got $other")
      },
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
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "setc",
      1,
      {
        case (ctx, Seq(c)) =>
          val n = number(c)
          val (newx, newy) = n match
            case q: QuaternionDouble     => (q.a, q.b)
            case q: QuaternionBigInt     => (q.a.doubleValue, q.b.doubleValue)
            case q: QuaternionRational   => (q.a.doubleValue, q.b.doubleValue)
            case c: ComplexDouble        => (c.re, c.im)
            case c: ComplexBigInt        => (c.re.doubleValue, c.im.doubleValue)
            case c: ComplexRational      => (c.re.doubleValue, c.im.doubleValue)
            case c: ComplexSmallRational => (c.re.doubleValue, c.im.doubleValue)
            case _                       => (n.doubleValue, 0.0)

          if ctx.pen then ctx.draws += DrawLine(ctx.x, ctx.y, newx, newy, ctx.color, ctx.width)
          ctx.x = newx
          ctx.y = newy
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "xcor",
      0,
      { case (ctx, _) => ctx.x },
    ),
    BuiltinProcedure(
      "ycor",
      0,
      { case (ctx, _) => ctx.y },
    ),
    BuiltinProcedure(
      "pos",
      0,
      { case (ctx, _) => ComplexDouble(ctx.x, ctx.y) },
    ),
    BuiltinProcedure(
      "heading",
      0,
      {
        case (ctx, _) =>
          val deg = math.toDegrees(Pi / 2 - ctx.heading)
          if deg < 0 then deg + 360 else deg
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
      "repcount",
      0,
      {
        case (ctx, _) =>
          if ctx.repcountStack.isEmpty then 0
          else ctx.repcountStack.top
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
      "stop",
      0,
      {
        case (ctx, _) => ctx.doStop()
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
    "rends"        -> "make",
    "rnd"          -> "random",
    "aleatoire"    -> "random",
    "alt"          -> "random",
    "se"           -> "sentence",
    "pr"           -> "print",
    "bf"           -> "butfirst",
    "bl"           -> "butlast",
    "empty?"       -> "emptyp",
    "list?"        -> "listp",
    "word?"        -> "wordp",
    "number?"      -> "numberp",
    "member?"      -> "memberp",
  ) map ((s, p) => s -> builtin(p)) toMap
