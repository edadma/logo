package io.github.edadma.logo

trait Draw
case class DrawLine(x1: Double, y1: Double, x2: Double, y2: Double, color: (Int, Int, Int), width: Double) extends Draw
case class DrawLabel(x: Double, y: Double, heading: Double, text: String)                                  extends Draw
// Arc: drawn from turtle position, heading is turtle's current heading (radians), angle is sweep in degrees, radius is arc radius
case class DrawArc(x: Double, y: Double, heading: Double, angle: Double, radius: Double, color: (Int, Int, Int), width: Double) extends Draw
