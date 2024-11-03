package io.github.edadma.logo

import java.io.ByteArrayOutputStream
import scala.Console.withOut

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
