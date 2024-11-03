package io.github.edadma.logo

trait Draw
case class DrawLine(x1: Double, y1: Double, x2: Double, y2: Double, color: String, width: Double) extends Draw
case class DrawLabel(x: Double, y: Double, heading: Double, text: String)                         extends Draw
