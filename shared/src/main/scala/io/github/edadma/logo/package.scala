package io.github.edadma.logo

private def logoNumber(n: Double): LogoNumber =
  val s =
    if n.isWhole then n.toInt.toString
    else n.toString

  LogoNumber(s, n)
