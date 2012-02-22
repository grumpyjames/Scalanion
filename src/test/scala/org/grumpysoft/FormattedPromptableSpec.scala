package org.grumpysoft

import org.specs2.mutable.Specification
import org.specs2.matcher.MatchResult

object FormattedPromptableSpec extends Specification {

  object Magic {
    val returnValue = 5;
  }

  trait DevNullPrompter extends Promptable {
    def prompt(message: SelfDescribing) {}
  }

  class TestPrompterWithFormatter(expectedGreeting: String, formatted: List[String], cannedFormatter: Formatter) extends DevNullPrompter {
    var result: MatchResult[_] = null
    def prompt(greeting: SelfDescribing, options: Seq[SelfDescribing]) : Seq[Int] = {
      val stringOptions = options.map(_.describe())
      result = (greeting.describe must_==expectedGreeting) and
        (stringOptions must_==formatted)
      List(Magic.returnValue);
    }

    protected def formatter() : Formatter = { cannedFormatter }
  }

  val unformatted = List(StringDescription("some"), StringDescription("bloody"), StringDescription("strings"))
  val formatted = List("formatted", "strings")
  val greeting = "Hello"

  val cannedFormatter = new Formatter() {
    def format(options: Seq[SelfDescribing]) = {
      options must_==unformatted
      formatted.map(StringDescription(_))
    }
  }
  val prompter = new TestPrompterWithFormatter(greeting, formatted, cannedFormatter) with FormattedPrompts

  "a formatted prompter" should {
    "format things correctly" in {
      (prompter.prompt(StringDescription(greeting), unformatted).head must_== Magic.returnValue) and (prompter.result)
    }
  }

}












