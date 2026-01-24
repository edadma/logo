package io.github.edadma.logo

trait Draw
// None = use current foreground color (theme-aware), Some(rgb) = explicit color
case class DrawSetColor(color: Option[(Int, Int, Int)]) extends Draw
case class DrawSetWidth(width: Double)                  extends Draw
case class DrawLine(x1: Double, y1: Double, x2: Double, y2: Double) extends Draw
case class DrawLabel(x: Double, y: Double, heading: Double, text: String) extends Draw
// Arc: drawn from turtle position, heading is turtle's current heading (radians), angle is sweep in degrees
case class DrawArc(x: Double, y: Double, heading: Double, angle: Double, radius: Double) extends Draw
