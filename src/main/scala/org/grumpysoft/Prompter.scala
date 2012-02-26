package org.grumpysoft

class Prompter(input: UserInput, output: Printer) extends Promptable {
  private def asSortedSetAsSeq(seq : Seq[Int]) : Seq[Int] = {
    seq.toSet.toList.sorted
  }


  def prompt(greeting: SelfDescribing, options : Seq[SelfDescribing]) : Seq[Int] = {
    asSortedSetAsSeq(readNext(greeting, options).dropWhile({response => invalidReturn(options, response)}).head)
  }

  def prompt(message: SelfDescribing) {
    output.println(message.describe())
  }

  private def invalidReturn(options : Seq[SelfDescribing], result : Seq[Int]) : Boolean = {
    result.exists({one => one > options.size || one <= 0})
  }

  private def readNext(greeting: SelfDescribing, options: Seq[SelfDescribing]) : Stream[Seq[Int]] = {
    doPrompt(greeting, options)
    Stream.cons(input.read(), readNext(greeting, options))
  }

  private def doPrompt(greeting: SelfDescribing, options: Seq[SelfDescribing]) {
    output.println(greeting.describe())
    options.foreach(opt => output.println(opt.describe()))
  }
}

class FormattedPrompter(input: UserInput, output: Printer, optionFormatter: Formatter) extends Prompter(input, output) with FormattedPrompts {
  protected def formatter() : Formatter = {
    optionFormatter
  }
}
