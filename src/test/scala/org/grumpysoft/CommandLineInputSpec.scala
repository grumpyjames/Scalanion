package org.grumpysoft

import java.io.BufferedReader
import java.io.ByteArrayInputStream
import java.io.InputStreamReader
import org.specs2.mutable.Specification

object CommandLineInputSpec extends Specification {
  def makeInputToRead(raw: String): StreamedUserInput = {
    val bytes = raw.getBytes
    val stream = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(bytes)))
    new StreamedUserInput(stream)
  }

  "a streamed user input, when asked for an int" should {
    "attempt to read, line by line, from the given stream, until a reasonable input is received" in {
      val cli = makeInputToRead("aaga\r\nfsdfnlkn\n3\r\n")
      cli.read must_==List(3)
    }

    "read multiple ints easily" in {
      val cli = makeInputToRead("adfsa\r\nfsdfnlkn\n1,3\r\n")
      cli.read must_==List(1,3)
    }

    "read an empty line as an empty list" in {
      val cli = makeInputToRead("\n")
      cli.read must_==List()
    }
  }
}
