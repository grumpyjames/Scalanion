import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

package org {
  package grumpysoft {

    trait Printer {
      def println(s: String) : Unit; 
    }

    class FakePrinter extends Printer {
      var printedLines : List[String] = List()
      def println(s: String) : Unit = { printedLines = s :: printedLines }
    }

    class CommandLinePlayer(output: Printer) {
      def prompt(options : Seq[String]) : Unit = {
	options.indices.zip(options).foreach(promptOne(_))
      }

      private def promptOne(optionWithIndex : (Int, String)) : Unit = {
	output.println(String.valueOf(optionWithIndex._1 + 1) + ". " + optionWithIndex._2)
      }
    }

    class CommandLinePlayerTest extends WordSpec {
      "a command line player" when {
	"prompted to choose from some options" should {
	  val printer = new FakePrinter()
	  val player = new CommandLinePlayer(printer)
	  "print those options to the supplied printstream" in {
	    val index = player.prompt(List("one","two","three"))
	    assert(printer.printedLines.reverse === List("1. one", "2. two", "3. three"))
	  }
	}
      }      
    }
  }  
}
