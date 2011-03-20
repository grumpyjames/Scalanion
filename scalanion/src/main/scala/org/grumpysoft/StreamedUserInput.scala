package org.grumpysoft

import java.io.BufferedReader

class StreamedUserInput(val input: BufferedReader) extends UserInput {
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
      val line = input.readLine
      line.length match {
        case 0 => Some(List())
        case _ => Some(line.split(",").toList.map(_.trim).map(Integer.parseInt(_)))
      }
    } catch {
      case e: NumberFormatException => None
      case unknown => throw unknown
    }
  }
}
