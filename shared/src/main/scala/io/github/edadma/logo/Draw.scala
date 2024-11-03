package io.github.edadma.logo

trait Draw
case class Line(x1: Double, y1: Double, x2: Double, y2: Double, color: String, width: Double) extends Draw
