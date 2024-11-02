package io.github.edadma.logo

trait Draw
case class Line(from: (Double, Double), to: (Double, Double), color: String) extends Draw
