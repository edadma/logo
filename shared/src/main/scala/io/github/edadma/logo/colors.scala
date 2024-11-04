package io.github.edadma.logo

import java.awt.Color

private val colors: Seq[(String, (Int, Int, Int))] = Seq(
  "black"        -> (0, 0, 0),
  "blue"         -> (0, 0, 255),
  "green"        -> (0, 255, 0),
  "cyan"         -> (0, 255, 255),
  "red"          -> (255, 0, 0),
  "magenta"      -> (255, 0, 255),
  "yellow"       -> (255, 255, 0),
  "white"        -> (255, 255, 255),
  "brown"        -> (139, 69, 19),
  "lightgray"    -> (211, 211, 211),
  "darkgray"     -> (169, 169, 169),
  "lightblue"    -> (173, 216, 230),
  "lightgreen"   -> (144, 238, 144),
  "lightcyan"    -> (224, 255, 255),
  "lightred"     -> (255, 182, 193),
  "lightmagenta" -> (238, 130, 238),
)

val colorMap = colors.toMap

val colorArray = colors.map(_._2).toVector
