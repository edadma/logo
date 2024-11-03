package io.github.edadma.logo

import java.io.ByteArrayOutputStream
import scala.Console.withOut
import scala.language.postfixOps

trait Test:
  def captureStdOut(block: => Unit): String =
    val outputStream = new ByteArrayOutputStream()

    withOut(outputStream) {
      block // Execute the block with stdout redirected
    }

    outputStream.toString

  def eval(code: String): String =
    val l = new Logo { override def event(): Unit = () }

    l.interp(code).toString

  def run(code: String): String =
    val l = new Logo {
      override def event(): Unit = ()
    }

    captureStdOut { l.interp(code) } trim
