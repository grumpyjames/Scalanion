package org.grumpysoft

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers._

import java.io.BufferedReader
import java.io.ByteArrayInputStream
import java.io.InputStreamReader

class StreamedUserInputTest extends WordSpec {
  "a streamed user input" when {
    "asked for an int" should {
      "attempt to read, line by line, from the given stream, until a reasonable input is received" in {
	val bytes = "aaga\r\nfsdfnlkn\n3\r\n".getBytes
 	val stream = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(bytes)))
 	val cli = new StreamedUserInput(stream)
 	cli.read should equal(3)
      }
    }
  }
}
