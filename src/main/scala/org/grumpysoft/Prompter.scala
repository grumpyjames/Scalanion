package org {
  package grumpysoft {
    class Prompter(input: UserInput, output: Printer) extends Promptable {
      def prompt(greeting: SelfDescribing, options : Seq[SelfDescribing]) : Seq[Int] = {
	uniquery.toSet(readNext(greeting, options).dropWhile({response => invalidReturn(options, response)}).head)
      }

      def prompt(message: SelfDescribing) : Unit = {
	output.println(message.describe)
      }

      private def invalidReturn(options : Seq[SelfDescribing], result : Seq[Int]) : Boolean = {
	result.exists({one => one > options.size || one <= 0})
      }

      private def readNext(greeting: SelfDescribing, options: Seq[SelfDescribing]) : Stream[Seq[Int]] = {
	doPrompt(greeting, options)
	Stream.cons(input.read, readNext(greeting, options))
      }
      
      private def doPrompt(greeting: SelfDescribing, options: Seq[SelfDescribing]) : Unit = {
	output.println(greeting.describe)	
	options.foreach(selfDescribing => output.println(selfDescribing.describe))
      }
    }

    // TODO: this must be useful elsewhere!
    object uniquery {
      def toSet[A](stream : Seq[A]) : Seq[A] = {
	val aList : List[A] = List()
	stream.foldLeft(aList) ( (acc,el) =>
	  if (acc.contains(el)) acc else el :: acc
			       ).reverse
      }
    }

    class FormattedPrompter(input: UserInput, output: Printer, optionFormatter: Formatter) extends Prompter(input, output) with FormattedPrompts {
      protected def formatter() : Formatter = {
	optionFormatter
      }
    }
  }
}
