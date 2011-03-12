package org.grumpysoft

import java.io.BufferedReader

class StreamedUserInput(val input: BufferedReader) {
  def read() : Seq[Int] = {
    readNext.dropWhile({ a =>
      a match {
        case None => true
        case Some(n) => false
      }
    }).head.get
  }

  private def readNext() : Stream[Option[List[Int]]] = {
    Stream.cons(readOne(), readNext())
  }

  private def readOne() : Option[List[Int]] = {
    try {
      Some(input.readLine.split(",").toList.map(_.trim).map(Integer.parseInt(_)))
    } catch {
      case e: NumberFormatException => None
      case unknown => throw unknown
    }
  }
}
