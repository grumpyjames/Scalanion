package org.grumpysoft

import java.io.BufferedReader
import java.io.ByteArrayInputStream
import java.io.InputStreamReader
import org.specs.Specification

object CommandLineInputSpec extends Specification {
  "a streamed user input, when asked for an int" should {
    "attempt to read, line by line, from the given stream, until a reasonable input is received" in {
      val bytes = "aaga\r\nfsdfnlkn\n3\r\n".getBytes
      val stream = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(bytes)))
      val cli = new StreamedUserInput(stream)
      cli.read must_==3
    }
  }
}
