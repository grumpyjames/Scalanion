package org.grumpysoft

import java.io.BufferedReader

class StreamedUserInput(val input: BufferedReader) {
  def read() : Int = {
    readNext.dropWhile({ a =>
      a match {
        case None => true
        case Some(n) => false
      }
    }).head.get
  }

  private def readNext() : Stream[Option[Int]] = {
    Stream.cons(readOne(), readNext())
  }

  private def readOne() : Option[Int] = {
    try {
      Some(Integer.parseInt(input.readLine))
    } catch {
      case e: NumberFormatException => None
      case unknown => throw unknown
    }
  }
}
