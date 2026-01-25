package io.github.edadma.logo

import scala.language.postfixOps

trait Test:
  def eval(code: String): String =
    val l = new Logo { override def event(): Unit = () }

    l.interp(code).toString

  def run(code: String): String =
    val output = new StringBuilder
    val l = new Logo { override def event(): Unit = () }

    l.setOutputHandler(s => output.append(s))
    l.interp(code)
    output.toString.trim
