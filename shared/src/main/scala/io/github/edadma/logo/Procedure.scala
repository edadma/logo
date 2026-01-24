package io.github.edadma.logo

import io.github.edadma.char_reader.CharReader
import io.github.edadma.dal.QuaternionDAL
import io.github.edadma.numbers.{ComplexDouble, ComplexBigInt, ComplexRational, ComplexSmallRational, QuaternionBigInt, QuaternionDouble, QuaternionRational}

import java.time.{LocalTime, LocalDate, ZoneOffset}

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

// State for gensym and random
private var gensymCounter: Long = 0

object RandomState:
  var generator: scala.util.Random = new scala.util.Random()

// UCB Logo template system for higher-order functions
object Template:
  // Apply a template to a single value, return the result
  def apply1(ctx: Logo, template: LogoValue, value: LogoValue): LogoValue =
    template match
      case LogoWord(procName) =>
        // Word template: call procedure with value as argument
        val code = s"$procName ${formatArg(value)}"
        ctx.interp(code)
      case LogoList(elems, _) =>
        // List template: substitute ? with value and run
        val substituted = elems.map(substituteOne(_, value))
        val code = substituted.map(formatValue).mkString(" ")
        ctx.interp(code)
      case _ =>
        problem(null, s"template must be a word or list, got $template")

  // Apply a binary template to two values (for reduce)
  def apply2(ctx: Logo, template: LogoValue, val1: LogoValue, val2: LogoValue): LogoValue =
    template match
      case LogoWord(procName) =>
        // Word template: call procedure with two arguments
        val code = s"$procName ${formatArg(val1)} ${formatArg(val2)}"
        ctx.interp(code)
      case LogoList(elems, _) =>
        // List template: substitute ?1 and ?2 (or first ? and second ?)
        val substituted = elems.map(substituteTwo(_, val1, val2))
        val code = substituted.map(formatValue).mkString(" ")
        ctx.interp(code)
      case _ =>
        problem(null, s"template must be a word or list, got $template")

  // Convert a value to its "code form" - words get quoted
  private def toCodeForm(value: LogoValue): LogoValue =
    value match
      case LogoWord(s)    => LogoWord("\"" + s)
      case LogoBoolean(b) => LogoWord(if b then "\"true" else "\"false")
      case other          => other

  // Substitute ? with value in a single element
  private def substituteOne(elem: LogoValue, value: LogoValue): LogoValue =
    elem match
      case LogoWord("?") => toCodeForm(value)
      case LogoWord(s) if s == "?1" => toCodeForm(value)
      case LogoList(inner, _) =>
        val subst = inner.map(substituteOne(_, value))
        LogoList(subst, subst :+ EOIToken())
      case other => other

  // Substitute ?1 and ?2 with values in a single element
  private def substituteTwo(elem: LogoValue, val1: LogoValue, val2: LogoValue): LogoValue =
    elem match
      case LogoWord("?") => toCodeForm(val1)      // First ? becomes ?1
      case LogoWord("?1") => toCodeForm(val1)
      case LogoWord("?2") => toCodeForm(val2)
      case LogoList(inner, _) =>
        val subst = inner.map(substituteTwo(_, val1, val2))
        LogoList(subst, subst :+ EOIToken())
      case other => other

  // Substitute ?n with value (for apply with multiple args)
  def substituteNth(elem: LogoValue, n: Int, value: LogoValue): LogoValue =
    elem match
      case LogoWord(s) if s == s"?$n" => toCodeForm(value)
      case LogoWord("?") if n == 1   => toCodeForm(value)
      case LogoList(inner, _) =>
        val subst = inner.map(substituteNth(_, n, value))
        LogoList(subst, subst :+ EOIToken())
      case other => other

  // Format a LogoValue for code generation
  def formatValue(v: LogoValue): String =
    v match
      case LogoWord(s)        => s
      case LogoNumber(n)      => n.toString
      case LogoBoolean(b)     => if b then "true" else "false"
      case LogoList(elems, _) => elems.map(formatValue).mkString("[", " ", "]")
      case _                  => v.toString

  // Format a value as an argument (add quote for words)
  def formatArg(v: LogoValue): String =
    v match
      case LogoWord(s)        => s"\"$s"
      case LogoNumber(n)      => n.toString
      case LogoBoolean(b)     => if b then "\"true" else "\"false"
      case LogoList(elems, _) => elems.map(formatValue).mkString("[", " ", "]")
      case _                  => v.toString

lazy val builtin: Map[String, Procedure] =
  List[Procedure](
    BuiltinFunction0("pi", () => Pi),
    BuiltinFunction0("e", () => E),
    BuiltinFunction0("i", () => QuaternionBigInt(0, 1, 0, 0)),
    BuiltinFunction0("j", () => QuaternionBigInt(0, 0, 1, 0)),
    BuiltinFunction0("k", () => QuaternionBigInt(0, 0, 0, 1)),
    BuiltinProcedure(
      "random",
      1,
      { case (_, Seq(limit)) => QuaternionDAL.compute("*", RandomState.generator.nextDouble(), number(limit)) },
    ),
    BuiltinVariadic(
      "print",
      1,
      1,
      (ctx, args) => { ctx.output(args.mkString(" ")); ctx.markYield() },
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
    // Workspace inspection procedures
    BuiltinProcedure(
      "namep",
      1,
      {
        case (ctx, Seq(name)) =>
          ctx.vars.contains(name.toString.toLowerCase)
      },
    ),
    BuiltinProcedure(
      "definedp",
      1,
      {
        case (ctx, Seq(name)) =>
          val lower = name.toString.toLowerCase
          ctx.procedures.contains(lower) || builtin.contains(lower) || synonyms.contains(lower)
      },
    ),
    BuiltinProcedure(
      "primitivep",
      1,
      {
        case (_, Seq(name)) =>
          val lower = name.toString.toLowerCase
          builtin.contains(lower) || synonyms.contains(lower)
      },
    ),
    BuiltinProcedure(
      "procedurep",
      1,
      {
        case (ctx, Seq(name)) =>
          ctx.procedures.contains(name.toString.toLowerCase)
      },
    ),
    BuiltinProcedure(
      "procedures",
      0,
      {
        case (ctx, _) =>
          val names = ctx.procedures.keys.toSeq.sorted.map(LogoWord(_))
          LogoList(names, names :+ EOIToken())
      },
    ),
    BuiltinProcedure(
      "primitives",
      0,
      {
        case (_, _) =>
          val names = (builtin.keys ++ synonyms.keys).toSeq.distinct.sorted.map(LogoWord(_))
          LogoList(names, names :+ EOIToken())
      },
    ),
    BuiltinProcedure(
      "names",
      0,
      {
        case (ctx, _) =>
          // UCB Logo format: [[] [varname1 varname2 ...]]
          val varNames = ctx.vars.keys.toSeq.sorted.map(LogoWord(_))
          val emptyList = LogoList(Seq.empty, Seq(EOIToken()))
          val namesList = LogoList(varNames, varNames :+ EOIToken())
          LogoList(Seq(emptyList, namesList), Seq(emptyList, namesList, EOIToken()))
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
    // UCB Logo: beforep - word1 comes before word2 in ASCII collating sequence
    BuiltinProcedure(
      "beforep",
      2,
      {
        case (_, Seq(a, b)) => a.toString.compareTo(b.toString) < 0
      },
    ),
    // UCB Logo: substringp - word1 is substring of word2
    BuiltinProcedure(
      "substringp",
      2,
      {
        case (_, Seq(LogoWord(sub), LogoWord(str))) => str.contains(sub)
        case (_, Seq(a, b))                          => b.toString.contains(a.toString)
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
    // UCB Logo modulo: result has same sign as divisor (floored division)
    BuiltinProcedure(
      "modulo",
      2,
      {
        case (_, Seq(a, b)) =>
          val dividend = number(a).doubleValue
          val divisor  = number(b).doubleValue
          val rem      = dividend % divisor
          if (rem == 0 || (rem > 0) == (divisor > 0)) rem
          else rem + divisor
      },
    ),
    BuiltinFunction2("pow", QuaternionDAL.compute("^", _, _)),
    BuiltinFunction1("negate", QuaternionDAL.negate),
    // UCB Logo: sin/cos/tan take DEGREES, radsin/radcos/radtan take radians
    BuiltinFunction1("sin", n => QuaternionDAL.sinFunction(QuaternionDAL.compute("*", n, Pi / 180))),
    BuiltinFunction1("cos", n => QuaternionDAL.cosFunction(QuaternionDAL.compute("*", n, Pi / 180))),
    BuiltinFunction1("tan", n => QuaternionDAL.tanFunction(QuaternionDAL.compute("*", n, Pi / 180))),
    // Radian versions
    BuiltinFunction1("radsin", QuaternionDAL.sinFunction),
    BuiltinFunction1("radcos", QuaternionDAL.cosFunction),
    BuiltinFunction1("radtan", QuaternionDAL.tanFunction),
    BuiltinFunction1("sinh", QuaternionDAL.sinhFunction),
    BuiltinFunction1("cosh", QuaternionDAL.coshFunction),
    BuiltinFunction1("tanh", QuaternionDAL.tanhFunction),
    BuiltinFunction1("sqrt", QuaternionDAL.sqrtFunction),
    BuiltinFunction1("exp", QuaternionDAL.expFunction),
    BuiltinFunction1("ln", QuaternionDAL.lnFunction),
    BuiltinFunction1("log10", n => QuaternionDAL.compute("/", QuaternionDAL.lnFunction(n), math.log(10))),
    // UCB Logo: asin/acos/atan return DEGREES
    BuiltinFunction1("asin", n => QuaternionDAL.compute("*", QuaternionDAL.asinFunction(n), 180 / Pi)),
    BuiltinFunction1("acos", n => QuaternionDAL.compute("*", QuaternionDAL.acosFunction(n), 180 / Pi)),
    BuiltinFunction1("atan", n => QuaternionDAL.compute("*", QuaternionDAL.atanFunction(n), 180 / Pi)),
    BuiltinFunction2("atan2", (y, x) => math.toDegrees(math.atan2(y.doubleValue, x.doubleValue))),
    // Radian versions
    BuiltinFunction1("radarcsin", QuaternionDAL.asinFunction),
    BuiltinFunction1("radarccos", QuaternionDAL.acosFunction),
    BuiltinFunction1("radarctan", QuaternionDAL.atanFunction),
    // Bitwise operations
    BuiltinProcedure(
      "bitand",
      2,
      { case (_, Seq(a, b)) => number(a).longValue & number(b).longValue },
    ),
    BuiltinProcedure(
      "bitor",
      2,
      { case (_, Seq(a, b)) => number(a).longValue | number(b).longValue },
    ),
    BuiltinProcedure(
      "bitxor",
      2,
      { case (_, Seq(a, b)) => number(a).longValue ^ number(b).longValue },
    ),
    BuiltinProcedure(
      "bitnot",
      1,
      { case (_, Seq(n)) => ~number(n).longValue },
    ),
    BuiltinProcedure(
      "ashift",
      2,
      {
        case (_, Seq(n, bits)) =>
          val num   = number(n).longValue
          val shift = number(bits).intValue
          if shift >= 0 then num << shift else num >> -shift // arithmetic shift
      },
    ),
    BuiltinProcedure(
      "lshift",
      2,
      {
        case (_, Seq(n, bits)) =>
          val num   = number(n).longValue
          val shift = number(bits).intValue
          if shift >= 0 then num << shift else num >>> -shift // logical shift
      },
    ),
    // Numeric functions
    BuiltinProcedure(
      "abs",
      1,
      { case (_, Seq(n)) => math.abs(number(n).doubleValue) },
    ),
    BuiltinProcedure(
      "int",
      1,
      { case (_, Seq(n)) => number(n).longValue },
    ),
    BuiltinProcedure(
      "round",
      1,
      { case (_, Seq(n)) => math.round(number(n).doubleValue) },
    ),
    BuiltinProcedure(
      "floor",
      1,
      { case (_, Seq(n)) => math.floor(number(n).doubleValue).toLong },
    ),
    BuiltinProcedure(
      "ceiling",
      1,
      { case (_, Seq(n)) => math.ceil(number(n).doubleValue).toLong },
    ),
    BuiltinProcedure(
      "sign",
      1,
      { case (_, Seq(n)) => math.signum(number(n).doubleValue).toInt },
    ),
    BuiltinVariadic(
      "min",
      2,
      2,
      (_, args) => args.map(number).reduce((a, b) => if QuaternionDAL.relate("<", a, b) then a else b),
    ),
    BuiltinVariadic(
      "max",
      2,
      2,
      (_, args) => args.map(number).reduce((a, b) => if QuaternionDAL.relate(">", a, b) then a else b),
    ),
    // Character/ASCII conversion
    BuiltinProcedure(
      "ascii",
      1,
      {
        case (_, Seq(LogoWord(s))) if s.nonEmpty => s.head.toInt
        case (_, Seq(other))                     => problem(null, s"'ascii' requires a non-empty word, got $other")
      },
    ),
    BuiltinProcedure(
      "char",
      1,
      { case (_, Seq(n)) => LogoWord(number(n).intValue.toChar.toString) },
    ),
    BuiltinProcedure(
      "lowercase",
      1,
      {
        case (_, Seq(LogoWord(s))) => LogoWord(s.toLowerCase)
        case (_, Seq(other))       => problem(null, s"'lowercase' requires a word, got $other")
      },
    ),
    BuiltinProcedure(
      "uppercase",
      1,
      {
        case (_, Seq(LogoWord(s))) => LogoWord(s.toUpperCase)
        case (_, Seq(other))       => problem(null, s"'uppercase' requires a word, got $other")
      },
    ),
    // List/word operations
    BuiltinProcedure(
      "reverse",
      1,
      {
        case (_, Seq(LogoList(elems, _))) =>
          val rev = elems.reverse
          LogoList(rev, rev :+ EOIToken())
        case (_, Seq(LogoWord(s))) => LogoWord(s.reverse)
        case (_, Seq(other))       => problem(null, s"'reverse' requires a list or word, got $other")
      },
    ),
    BuiltinProcedure(
      "pick",
      1,
      {
        case (_, Seq(LogoList(elems, _))) =>
          if elems.isEmpty then problem(null, "'pick' requires non-empty list")
          else elems(scala.util.Random.nextInt(elems.length))
        case (_, Seq(LogoWord(s))) =>
          if s.isEmpty then problem(null, "'pick' requires non-empty word")
          else LogoWord(s(scala.util.Random.nextInt(s.length)).toString)
        case (_, Seq(other)) => problem(null, s"'pick' requires a list or word, got $other")
      },
    ),
    BuiltinVariadic(
      "range",
      1,
      1,
      (_, args) =>
        val nums = args.map(number)
        val (from, to, step) = nums match
          case Seq(end)             => (0L, end.longValue, 1L)
          case Seq(start, end)      => (start.longValue, end.longValue, 1L)
          case Seq(start, end, stp) => (start.longValue, end.longValue, stp.longValue)
          case _ => problem(null, "'range' takes 1 to 3 arguments: [from] to [step]")
        if step == 0 then problem(null, "'range' step cannot be zero")
        val elems: Seq[LogoValue] =
          if step > 0 then (from until to by step).map(n => LogoNumber(n))
          else (from until to by step).map(n => LogoNumber(n))
        LogoList(elems, elems :+ EOIToken()),
    ),
    // UCB Logo: member returns tail starting with item, or empty if not found
    BuiltinProcedure(
      "member",
      2,
      {
        case (_, Seq(elem, LogoList(elems, _))) =>
          val idx = elems.indexOf(elem)
          if idx < 0 then LogoList(Seq.empty, Seq(EOIToken()))
          else
            val tail = elems.drop(idx)
            LogoList(tail, tail :+ EOIToken())
        case (_, Seq(LogoWord(c), LogoWord(s))) =>
          val idx = s.indexOf(c)
          if idx < 0 then LogoWord("")
          else LogoWord(s.substring(idx))
        case (_, Seq(_, other)) => problem(null, s"'member' requires a list or word as second argument, got $other")
      },
    ),
    // UCB Logo: remove all occurrences of thing from list/word
    BuiltinProcedure(
      "remove",
      2,
      {
        case (_, Seq(elem, LogoList(elems, _))) =>
          val filtered = elems.filterNot(_ == elem)
          LogoList(filtered, filtered :+ EOIToken())
        case (_, Seq(LogoWord(c), LogoWord(s))) =>
          LogoWord(s.filterNot(ch => ch.toString == c))
        case (_, Seq(_, other)) => problem(null, s"'remove' requires a list or word as second argument, got $other")
      },
    ),
    // UCB Logo: remove duplicate elements from list
    BuiltinProcedure(
      "remdup",
      1,
      {
        case (_, Seq(LogoList(elems, _))) =>
          val unique = elems.distinct
          LogoList(unique, unique :+ EOIToken())
        case (_, Seq(LogoWord(s))) =>
          LogoWord(s.distinct)
        case (_, Seq(other)) => problem(null, s"'remdup' requires a list or word, got $other")
      },
    ),
    // UCB Logo: combine - if second arg is word, concatenate; if list, fput
    BuiltinProcedure(
      "combine",
      2,
      {
        case (_, Seq(LogoWord(a), LogoWord(b))) => LogoWord(a + b)
        case (_, Seq(elem, LogoList(elems, _))) =>
          val newList = elem +: elems
          LogoList(newList, newList :+ EOIToken())
        case (_, Seq(a, b)) => problem(null, s"'combine' invalid arguments: $a, $b")
      },
    ),
    // UCB Logo: firsts - list of first of each member
    BuiltinProcedure(
      "firsts",
      1,
      {
        case (_, Seq(LogoList(elems, _))) =>
          val firsts = elems.map {
            case LogoList(inner, _) if inner.nonEmpty => inner.head
            case LogoWord(s) if s.nonEmpty           => LogoWord(s.head.toString)
            case other                               => problem(null, s"'firsts' element has no first: $other")
          }
          LogoList(firsts, firsts :+ EOIToken())
        case (_, Seq(other)) => problem(null, s"'firsts' requires a list, got $other")
      },
    ),
    // UCB Logo: butfirsts - list of butfirst of each member
    BuiltinProcedure(
      "butfirsts",
      1,
      {
        case (_, Seq(LogoList(elems, _))) =>
          val bfs = elems.map {
            case LogoList(inner, _) if inner.nonEmpty =>
              val rest = inner.tail
              LogoList(rest, rest :+ EOIToken())
            case LogoWord(s) if s.nonEmpty => LogoWord(s.tail)
            case other                     => problem(null, s"'butfirsts' element has no butfirst: $other")
          }
          LogoList(bfs, bfs :+ EOIToken())
        case (_, Seq(other)) => problem(null, s"'butfirsts' requires a list, got $other")
      },
    ),
    // UCB Logo: gensym - generate unique symbol G1, G2, G3...
    BuiltinProcedure(
      "gensym",
      0,
      {
        case (_, _) =>
          gensymCounter += 1
          LogoWord(s"G$gensymCounter")
      },
    ),
    // UCB Logo: quoted - prepend quote to word
    BuiltinProcedure(
      "quoted",
      1,
      {
        case (_, Seq(LogoWord(s))) => LogoWord("\"" + s)
        case (_, Seq(v))           => LogoWord("\"" + v.toString)
      },
    ),
    // UCB Logo: rerandom - reset random number generator (optionally with seed)
    // defaultArgs=1 means it takes 1 arg when called normally, minArgs=0 allows (rerandom)
    BuiltinVariadic(
      "rerandom",
      1,
      0,
      (_, args) =>
        args match
          case Seq() =>
            RandomState.generator = new scala.util.Random()
          case Seq(seed) =>
            RandomState.generator = new scala.util.Random(number(seed).longValue)
          case _ => problem(null, "'rerandom' takes 0 or 1 argument")
        LogoNull(),
    ),
    // UCB Logo: rseq - real sequence (like iseq but returns decimals)
    BuiltinProcedure(
      "rseq",
      3,
      {
        case (_, Seq(from, to, count)) =>
          val start = number(from).doubleValue
          val end   = number(to).doubleValue
          val n     = number(count).intValue
          if n < 1 then problem(null, "'rseq' count must be at least 1")
          val step  = if n == 1 then 0.0 else (end - start) / (n - 1)
          val elems = (0 until n).map(i => LogoNumber(start + i * step))
          LogoList(elems, elems :+ EOIToken())
      },
    ),
    // UCB Logo: setitem - destructively set item in list variable
    // setitem index varname value
    BuiltinProcedure(
      "setitem",
      3,
      {
        case (ctx, Seq(idx, varName, value)) =>
          val i       = number(idx).intValue
          val name    = varName.toString.toLowerCase
          val listVal = ctx.vars.getOrElse(name, problem(null, s"'setitem' unknown variable '$name'"))
          listVal match
            case LogoList(elems, _) =>
              if i < 1 || i > elems.length then problem(null, s"'setitem' index $i out of range 1..${elems.length}")
              val newElems = elems.updated(i - 1, value)
              ctx.vars(name) = LogoList(newElems, newElems :+ EOIToken())
              LogoNull()
            case _ => problem(null, s"'setitem' variable '$name' is not a list")
      },
    ),
    // UCB Logo: push - add to front of list in variable
    // push "varname value
    BuiltinProcedure(
      "push",
      2,
      {
        case (ctx, Seq(varName, value)) =>
          val name    = varName.toString.toLowerCase
          val listVal = ctx.vars.getOrElse(name, problem(null, s"'push' unknown variable '$name'"))
          listVal match
            case LogoList(elems, _) =>
              val newElems = value +: elems
              ctx.vars(name) = LogoList(newElems, newElems :+ EOIToken())
              LogoNull()
            case _ => problem(null, s"'push' variable '$name' is not a list")
      },
    ),
    // UCB Logo: pop - remove and return first element from list in variable
    // pop "varname
    BuiltinProcedure(
      "pop",
      1,
      {
        case (ctx, Seq(varName)) =>
          val name    = varName.toString.toLowerCase
          val listVal = ctx.vars.getOrElse(name, problem(null, s"'pop' unknown variable '$name'"))
          listVal match
            case LogoList(elems, _) =>
              if elems.isEmpty then problem(null, "'pop' cannot pop from empty list")
              val first    = elems.head
              val newElems = elems.tail
              ctx.vars(name) = LogoList(newElems, newElems :+ EOIToken())
              first
            case _ => problem(null, s"'pop' variable '$name' is not a list")
      },
    ),
    // UCB Logo: queue - add to end of list in variable
    // queue "varname value
    BuiltinProcedure(
      "queue",
      2,
      {
        case (ctx, Seq(varName, value)) =>
          val name    = varName.toString.toLowerCase
          val listVal = ctx.vars.getOrElse(name, problem(null, s"'queue' unknown variable '$name'"))
          listVal match
            case LogoList(elems, _) =>
              val newElems = elems :+ value
              ctx.vars(name) = LogoList(newElems, newElems :+ EOIToken())
              LogoNull()
            case _ => problem(null, s"'queue' variable '$name' is not a list")
      },
    ),
    // UCB Logo: dequeue - same as pop (remove and return first element)
    // dequeue "varname
    BuiltinProcedure(
      "dequeue",
      1,
      {
        case (ctx, Seq(varName)) =>
          val name    = varName.toString.toLowerCase
          val listVal = ctx.vars.getOrElse(name, problem(null, s"'dequeue' unknown variable '$name'"))
          listVal match
            case LogoList(elems, _) =>
              if elems.isEmpty then problem(null, "'dequeue' cannot dequeue from empty list")
              val first    = elems.head
              val newElems = elems.tail
              ctx.vars(name) = LogoList(newElems, newElems :+ EOIToken())
              first
            case _ => problem(null, s"'dequeue' variable '$name' is not a list")
      },
    ),
    // ============================================================================
    // UCB Logo Higher-Order Functions (Template-based)
    // ============================================================================
    // APPLY template list - call procedure/template with list items as arguments
    BuiltinProcedure(
      "apply",
      2,
      {
        case (ctx, Seq(template, LogoList(args, _))) =>
          template match
            case LogoWord(procName) =>
              // Call procedure with args - use parentheses for variadic support
              val argStr = args.map(Template.formatArg).mkString(" ")
              val code = s"($procName $argStr)"
              ctx.interp(code)
            case LogoList(elems, _) =>
              // List template - substitute ?1, ?2, ... with args
              val substituted = elems.map { elem =>
                args.zipWithIndex.foldLeft(elem) { case (e, (arg, i)) =>
                  Template.substituteNth(e, i + 1, arg)
                }
              }
              val code = substituted.map(Template.formatValue).mkString(" ")
              ctx.interp(code)
            case _ => problem(null, "'apply' first argument must be a word or list template")
        case (_, Seq(_, other)) => problem(null, s"'apply' second argument must be a list, got $other")
      },
    ),
    // INVOKE procname list - same as apply but first arg is explicitly a procedure name
    BuiltinProcedure(
      "invoke",
      2,
      {
        case (ctx, Seq(procName, LogoList(args, _))) =>
          // Use parentheses for variadic support
          val argStr = args.map(Template.formatArg).mkString(" ")
          val code = s"(${procName.toString} $argStr)"
          ctx.interp(code)
        case (_, Seq(_, other)) => problem(null, s"'invoke' second argument must be a list, got $other")
      },
    ),
    // FOREACH template list - apply template to each element for side effects
    BuiltinProcedure(
      "foreach",
      2,
      {
        case (ctx, Seq(template, LogoList(elems, _))) =>
          elems.foreach(elem => Template.apply1(ctx, template, elem))
          LogoNull()
        case (_, Seq(_, other)) => problem(null, s"'foreach' second argument must be a list, got $other")
      },
    ),
    // MAP template list - apply template to each element, return list of results
    BuiltinProcedure(
      "map",
      2,
      {
        case (ctx, Seq(template, LogoList(elems, _))) =>
          val results = elems.map(elem => Template.apply1(ctx, template, elem))
          LogoList(results, results :+ EOIToken())
        case (_, Seq(_, other)) => problem(null, s"'map' second argument must be a list, got $other")
      },
    ),
    // MAP.SE template list - like map but flattens results with sentence
    BuiltinProcedure(
      "map.se",
      2,
      {
        case (ctx, Seq(template, LogoList(elems, _))) =>
          val results = elems.flatMap { elem =>
            Template.apply1(ctx, template, elem) match
              case LogoList(inner, _) => inner
              case other              => Seq(other)
          }
          LogoList(results, results :+ EOIToken())
        case (_, Seq(_, other)) => problem(null, s"'map.se' second argument must be a list, got $other")
      },
    ),
    // FILTER template list - keep elements where template returns true
    BuiltinProcedure(
      "filter",
      2,
      {
        case (ctx, Seq(template, LogoList(elems, _))) =>
          val results = elems.filter { elem =>
            Template.apply1(ctx, template, elem) match
              case LogoBoolean(b) => b
              case other          => problem(null, s"'filter' template must return true/false, got $other")
          }
          LogoList(results, results :+ EOIToken())
        case (_, Seq(_, other)) => problem(null, s"'filter' second argument must be a list, got $other")
      },
    ),
    // FIND template list - return first element where template returns true, or empty list
    BuiltinProcedure(
      "find",
      2,
      {
        case (ctx, Seq(template, LogoList(elems, _))) =>
          elems.find { elem =>
            Template.apply1(ctx, template, elem) match
              case LogoBoolean(b) => b
              case other          => problem(null, s"'find' template must return true/false, got $other")
          } match
            case Some(found) => found
            case None        => LogoList(Seq.empty, Seq(EOIToken()))
        case (_, Seq(_, other)) => problem(null, s"'find' second argument must be a list, got $other")
      },
    ),
    // REDUCE template list - fold list with binary template
    BuiltinProcedure(
      "reduce",
      2,
      {
        case (ctx, Seq(template, LogoList(elems, _))) =>
          if elems.isEmpty then problem(null, "'reduce' requires non-empty list")
          elems.tail.foldLeft(elems.head) { (acc, elem) =>
            Template.apply2(ctx, template, acc, elem)
          }
        case (_, Seq(_, other)) => problem(null, s"'reduce' second argument must be a list, got $other")
      },
    ),
    // Variable access
    BuiltinProcedure(
      "thing",
      1,
      {
        case (ctx, Seq(name)) =>
          val varName = name.toString.toLowerCase
          ctx.vars.get(varName) match
            case Some(v) => v
            case None    => problem(null, s"'thing' unknown variable '$varName'")
      },
    ),
    // Output procedures
    BuiltinVariadic(
      "type",
      1,
      1,
      (ctx, args) => { ctx.outputNoNewline(args.mkString(" ")); ctx.markYield() },
    ),
    BuiltinVariadic(
      "show",
      1,
      1,
      (ctx, args) => {
        ctx.output(
          args
            .map {
              case LogoWord(s)        => s"\"$s"
              case LogoList(elems, _) => elems.mkString("[", " ", "]")
              case v                  => v.toString
            }
            .mkString(" "),
        )
        ctx.markYield()
      },
    ),
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
          val (rawX, rawY) = ctx.computeEndpoint(number(distance).doubleValue)
          val (x2, y2) = ctx.applyScreenMode(rawX, rawY)

          if ctx.pen then { ctx.emitStyleChanges(); ctx.draws += DrawLine(ctx.x, ctx.y, x2, y2) }
          ctx.x = x2
          ctx.y = y2
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "right",
      1,
      {
        case (ctx, Seq(turn)) =>
          ctx.heading = ctx.computeTurn(number(turn).doubleValue)
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "back",
      1,
      {
        case (ctx, Seq(distance)) =>
          val (rawX, rawY) = ctx.computeEndpoint(-number(distance).doubleValue)
          val (x2, y2) = ctx.applyScreenMode(rawX, rawY)

          if ctx.pen then { ctx.emitStyleChanges(); ctx.draws += DrawLine(ctx.x, ctx.y, x2, y2) }
          ctx.x = x2
          ctx.y = y2
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "left",
      1,
      {
        case (ctx, Seq(turn)) =>
          ctx.heading = ctx.computeTurn(-number(turn).doubleValue)
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "label",
      1,
      {
        case (ctx, Seq(text)) =>
          ctx.draws += DrawLabel(ctx.x, ctx.y, ctx.heading, text.toString)
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "setpensize",
      1,
      {
        case (ctx, Seq(size @ LogoList(Seq(width, _), _))) =>
          ctx.width = number(width).doubleValue
          ctx.markYield()
          ctx.event()
        case (ctx, Seq(width)) =>
          ctx.width = number(width).doubleValue
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "setcolor",
      1,
      {
        case (ctx, Seq(LogoNull())) =>
          ctx.color = ctx.defaultColor
          ctx.usingDefaultColor = true
          ctx.markYield()
          ctx.event()
        case (ctx, Seq(LogoList(Seq(r, g, b), _))) =>
          ctx.color = (number(r).intValue, number(g).intValue, number(b).intValue)
          ctx.usingDefaultColor = false
          ctx.markYield()
          ctx.event()
        case (ctx, Seq(LogoNumber(n))) =>
          ctx.color = colorArray(n.intValue)
          ctx.usingDefaultColor = false
          ctx.markYield()
          ctx.event()
        case (ctx, Seq(LogoWord(c))) =>
          ctx.color = colorMap(c)
          ctx.usingDefaultColor = false
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "clearscreen",
      0,
      {
        case (ctx, _) =>
          ctx.clearscreen()
          ctx.markYield()
          ctx.event()
      },
    ),
    // clean: erase graphics without moving turtle
    BuiltinProcedure(
      "clean",
      0,
      {
        case (ctx, _) =>
          ctx.clean()
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "home",
      0,
      {
        case (ctx, _) =>
          ctx.home()
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "setxy",
      2,
      {
        case (ctx, Seq(x, y)) =>
          val rawX = number(x).doubleValue
          val rawY = number(y).doubleValue
          val (newx, newy) = ctx.applyScreenMode(rawX, rawY)

          if ctx.pen then { ctx.emitStyleChanges(); ctx.draws += DrawLine(ctx.x, ctx.y, newx, newy) }
          ctx.x = newx
          ctx.y = newy
          ctx.markYield()
          ctx.event()
      },
    ),
    // setpos: like setxy but takes a list [x y]
    BuiltinProcedure(
      "setpos",
      1,
      {
        case (ctx, Seq(LogoList(Seq(x, y), _))) =>
          val rawX = number(x).doubleValue
          val rawY = number(y).doubleValue
          val (newx, newy) = ctx.applyScreenMode(rawX, rawY)

          if ctx.pen then { ctx.emitStyleChanges(); ctx.draws += DrawLine(ctx.x, ctx.y, newx, newy) }
          ctx.x = newx
          ctx.y = newy
          ctx.markYield()
          ctx.event()
        case (_, Seq(other)) => problem(null, s"'setpos' requires a list [x y], got $other")
      },
    ),
    BuiltinProcedure(
      "setx",
      1,
      {
        case (ctx, Seq(x)) =>
          val rawX = number(x).doubleValue
          val (newx, _) = ctx.applyScreenMode(rawX, ctx.y)
          if ctx.pen then { ctx.emitStyleChanges(); ctx.draws += DrawLine(ctx.x, ctx.y, newx, ctx.y) }
          ctx.x = newx
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "sety",
      1,
      {
        case (ctx, Seq(y)) =>
          val rawY = number(y).doubleValue
          val (_, newy) = ctx.applyScreenMode(ctx.x, rawY)
          if ctx.pen then { ctx.emitStyleChanges(); ctx.draws += DrawLine(ctx.x, ctx.y, ctx.x, newy) }
          ctx.y = newy
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "setheading",
      1,
      {
        case (ctx, Seq(h)) =>
          ctx.heading = ctx.computeHeading(number(h).doubleValue)
          ctx.markYield()
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

          if ctx.pen then { ctx.emitStyleChanges(); ctx.draws += DrawLine(ctx.x, ctx.y, newx, newy) }
          ctx.x = newx
          ctx.y = newy
          ctx.markYield()
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
    // towards: heading toward a point
    BuiltinProcedure(
      "towards",
      1,
      {
        case (ctx, Seq(LogoList(Seq(tx, ty), _))) =>
          val targetX = number(tx).doubleValue
          val targetY = number(ty).doubleValue
          val dx = targetX - ctx.x
          val dy = targetY - ctx.y
          // Convert from math angle (radians, 0=east, CCW) to Logo heading (degrees, 0=north, CW)
          val angleRad = math.atan2(dy, dx)
          val logoDeg = 90 - math.toDegrees(angleRad)
          // Normalize to 0-360
          if logoDeg < 0 then logoDeg + 360 else if logoDeg >= 360 then logoDeg - 360 else logoDeg
        case (_, Seq(other)) => problem(null, s"'towards' requires a list [x y], got $other")
      },
    ),
    // distance: distance to a point
    BuiltinProcedure(
      "distance",
      1,
      {
        case (ctx, Seq(LogoList(Seq(tx, ty), _))) =>
          val targetX = number(tx).doubleValue
          val targetY = number(ty).doubleValue
          val dx = targetX - ctx.x
          val dy = targetY - ctx.y
          math.sqrt(dx * dx + dy * dy)
        case (_, Seq(other)) => problem(null, s"'distance' requires a list [x y], got $other")
      },
    ),
    // arc: draw an arc (angle in degrees, radius)
    BuiltinProcedure(
      "arc",
      2,
      {
        case (ctx, Seq(angle, radius)) =>
          val angleDeg = number(angle).doubleValue
          val r = number(radius).doubleValue
          if ctx.pen then
            ctx.emitStyleChanges()
            ctx.draws += DrawArc(ctx.x, ctx.y, ctx.heading, angleDeg, r)
          ctx.markYield()
          ctx.event()
      },
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
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "pendown",
      0,
      {
        case (ctx, _) =>
          ctx.pen = true
          ctx.markYield()
          ctx.event()
      },
    ),
    // Pen modes
    BuiltinProcedure(
      "penpaint",
      0,
      {
        case (ctx, _) =>
          ctx.pen = true
          ctx.penMode = PaintMode
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "penerase",
      0,
      {
        case (ctx, _) =>
          ctx.pen = true
          ctx.penMode = EraseMode
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "penreverse",
      0,
      {
        case (ctx, _) =>
          ctx.pen = true
          ctx.penMode = ReverseMode
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "penmode",
      0,
      {
        case (ctx, _) =>
          ctx.penMode match
            case PaintMode   => LogoWord("paint")
            case EraseMode   => LogoWord("erase")
            case ReverseMode => LogoWord("reverse")
      },
    ),
    // Pen queries
    BuiltinProcedure(
      "pensize",
      0,
      {
        case (ctx, _) =>
          val w = ctx.width
          val elems = Seq(LogoNumber(w), LogoNumber(w))
          LogoList(elems, elems :+ EOIToken())
      },
    ),
    BuiltinProcedure(
      "pencolor",
      0,
      {
        case (ctx, _) =>
          val (r, g, b) = ctx.color
          val elems = Seq(LogoNumber(r), LogoNumber(g), LogoNumber(b))
          LogoList(elems, elems :+ EOIToken())
      },
    ),
    // Background color
    BuiltinProcedure(
      "background",
      0,
      {
        case (ctx, _) =>
          val (r, g, b) = ctx.backgroundColor
          val elems = Seq(LogoNumber(r), LogoNumber(g), LogoNumber(b))
          LogoList(elems, elems :+ EOIToken())
      },
    ),
    BuiltinProcedure(
      "setbackground",
      1,
      {
        case (ctx, Seq(LogoList(Seq(r, g, b), _))) =>
          ctx.backgroundColor = (number(r).intValue, number(g).intValue, number(b).intValue)
          ctx.markYield()
          ctx.event()
        case (ctx, Seq(LogoNumber(n))) =>
          ctx.backgroundColor = colorArray(n.intValue)
          ctx.markYield()
          ctx.event()
        case (ctx, Seq(LogoWord(c))) =>
          ctx.backgroundColor = colorMap(c)
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "hideturtle",
      0,
      {
        case (ctx, _) =>
          ctx.show = false
          ctx.markYield()
          ctx.event()
      },
    ),
    BuiltinProcedure(
      "showturtle",
      0,
      {
        case (ctx, _) =>
          ctx.show = true
          ctx.markYield()
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
      "local",
      1,
      {
        case (ctx, Seq(name)) =>
          ctx.declareLocal(name.toString.toLowerCase)
          LogoNull()
      },
    ),
    BuiltinProcedure(
      "localmake",
      2,
      {
        case (ctx, Seq(name, value)) =>
          val varName = name.toString.toLowerCase
          ctx.declareLocal(varName)
          ctx.vars(varName) = value
          LogoNull()
      },
    ),
    BuiltinProcedure(
      "stop",
      0,
      {
        case (ctx, _) => ctx.doStop()
      },
    ),
    // Screen boundary modes
    BuiltinProcedure(
      "window",
      0,
      {
        case (ctx, _) =>
          ctx.screenMode = WindowMode
          LogoNull()
      },
    ),
    BuiltinProcedure(
      "fence",
      0,
      {
        case (ctx, _) =>
          ctx.screenMode = FenceMode
          LogoNull()
      },
    ),
    BuiltinProcedure(
      "wrap",
      0,
      {
        case (ctx, _) =>
          ctx.screenMode = WrapMode
          LogoNull()
      },
    ),
    BuiltinProcedure(
      "screenmode",
      0,
      {
        case (ctx, _) =>
          ctx.screenMode match
            case WindowMode => LogoWord("window")
            case FenceMode  => LogoWord("fence")
            case WrapMode   => LogoWord("wrap")
      },
    ),
    // pendownp / pendown? - true if pen is down
    BuiltinProcedure(
      "pendownp",
      0,
      {
        case (ctx, _) => ctx.pen
      },
    ),
    // shownp / shown? - true if turtle is visible
    BuiltinProcedure(
      "shownp",
      0,
      {
        case (ctx, _) => ctx.show
      },
    ),
    // Number formatting
    BuiltinProcedure(
      "form",
      3,
      {
        case (_, Seq(num, width, precision)) =>
          val n = number(num).doubleValue
          val w = number(width).intValue
          val p = number(precision).intValue
          val formatted = if p == 0 then f"${n.toLong}%d" else s"%.${p}f".format(n)
          val padded = if formatted.length >= w then formatted else " " * (w - formatted.length) + formatted
          LogoWord(padded)
      },
    ),
    // Test/iftrue/iffalse for flag-based conditionals
    BuiltinProcedure(
      "test",
      1,
      {
        case (ctx, Seq(tf)) =>
          val result = boolean(tf)
          if ctx.testResultStack.isEmpty then
            ctx.testResultStack.push(result)
          else
            ctx.testResultStack.pop()
            ctx.testResultStack.push(result)
          LogoNull()
      },
    ),
    // Time procedures (use UTC for cross-platform compatibility)
    BuiltinProcedure(
      "time",
      0,
      {
        case (_, _) =>
          val now = LocalTime.now(ZoneOffset.UTC)
          val elems = Seq(LogoNumber(now.getHour), LogoNumber(now.getMinute), LogoNumber(now.getSecond))
          LogoList(elems, elems :+ EOIToken())
      },
    ),
    BuiltinProcedure(
      "date",
      0,
      {
        case (_, _) =>
          val today = LocalDate.now(ZoneOffset.UTC)
          val elems = Seq(LogoNumber(today.getYear), LogoNumber(today.getMonthValue), LogoNumber(today.getDayOfMonth))
          LogoList(elems, elems :+ EOIToken())
      },
    ),
    BuiltinProcedure(
      "timemilli",
      0,
      {
        case (_, _) => System.currentTimeMillis()
      },
    ),
    // ignore - discard a value (useful for side-effect operations)
    BuiltinProcedure(
      "ignore",
      1,
      {
        case (_, _) => LogoNull()
      },
    ),
    // Input procedures
    BuiltinProcedure(
      "readlist",
      0,
      {
        case (ctx, _) =>
          val line = ctx.readLine()
          if line == null then LogoList(Seq.empty, Seq(EOIToken()))
          else
            val tokens = transform(tokenize(CharReader.fromString(line)))
            LogoList(tokens.filterNot(_.isInstanceOf[EOIToken]), tokens)
      },
    ),
    BuiltinProcedure(
      "readword",
      0,
      {
        case (ctx, _) =>
          val line = ctx.readLine()
          if line == null then LogoList(Seq.empty, Seq(EOIToken()))
          else LogoWord(line)
      },
    ),
    BuiltinProcedure(
      "readchar",
      0,
      {
        case (ctx, _) =>
          val ch = ctx.readChar()
          if ch == -1 then LogoList(Seq.empty, Seq(EOIToken()))
          else LogoWord(ch.toChar.toString)
      },
    ),
    // parse - convert a word to a list of tokens
    BuiltinProcedure(
      "parse",
      1,
      {
        case (_, Seq(word)) =>
          val text = word.toString
          val tokens = transform(tokenize(CharReader.fromString(text)))
          LogoList(tokens.filterNot(_.isInstanceOf[EOIToken]), tokens)
      },
    ),
    // runparse - like parse but also substitutes variables (colon expressions)
    BuiltinProcedure(
      "runparse",
      1,
      {
        case (ctx, Seq(wordOrList)) =>
          val text = wordOrList match
            case LogoList(elems, _) => elems.map(_.toString).mkString(" ")
            case other              => other.toString
          val tokens = transform(tokenize(CharReader.fromString(text)))
          // Substitute variables
          val substituted = tokens.map {
            case LogoWord(s) if s.startsWith(":") =>
              val varName = s.tail.toLowerCase
              ctx.vars.getOrElse(varName, LogoWord(s))
            case other => other
          }
          LogoList(substituted.filterNot(_.isInstanceOf[EOIToken]), substituted)
      },
    ),
  ) map (p => p.name -> p) toMap

lazy val synonyms: Map[String, Procedure] =
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
    "seth"         -> "setheading",
    "mod"          -> "remainder",
    "number?"      -> "numberp",
    "member?"      -> "memberp",
    "iseq"         -> "range",
    "arctan"       -> "atan",
    "arcsin"       -> "asin",
    "arccos"       -> "acos",
    "ceil"         -> "ceiling",
    "minus"        -> "negate",
    "before?"      -> "beforep",
    "substring?"   -> "substringp",
    "bfs"          -> "butfirsts",
    "equal?"       -> "equalp",
    "notequal?"    -> "notequalp",
    "less?"        -> "lessp",
    "greater?"     -> "greaterp",
    "lessequal?"   -> "lessequalp",
    "greaterequal?" -> "greaterequalp",
    "name?"         -> "namep",
    "defined?"      -> "definedp",
    "primitive?"    -> "primitivep",
    "procedure?"    -> "procedurep",
    "pendown?"      -> "pendownp",
    "shown?"        -> "shownp",
    // Pen mode synonyms
    "ppt"           -> "penpaint",
    "pe"            -> "penerase",
    "px"            -> "penreverse",
    "pc"            -> "pencolor",
    "bg"            -> "background",
    "setbg"         -> "setbackground",
  ) map ((s, p) => s -> builtin(p)) toMap
