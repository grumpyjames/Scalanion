package org {
  package grumpysoft {

  trait FormattedPrompts extends Promptable {
    abstract override def prompt(greeting: SelfDescribing, options: Seq[SelfDescribing]) : Seq[Int] = {
      val formattedOptions = formatter.format(options)
      super.prompt(greeting, formattedOptions)
    }

      protected def formatter() : Formatter;
    }

 }
}
