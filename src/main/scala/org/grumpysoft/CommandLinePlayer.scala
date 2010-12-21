package org.grumpysoft

class CommandLinePlayer(private val name: String, private val userInterface: Promptable) extends Player with SelfDescribing {

  def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[Int] = {
    nextInput(buildDescription(purpose, minChoices, maxChoices), cards).dropWhile(invalid(minChoices, maxChoices, _)).head
  }

  def nextInput(greeting: SelfDescribing, options: Seq[SelfDescribing]) : Stream[Seq[Int]] = {
    Stream.cons(userInterface.prompt(greeting, options), nextInput(greeting, options))
  }

  def invalid(minChoices: Int, maxChoices: Int, choices: Seq[Int]) : Boolean = {
    println("min: " + minChoices + ", max: " + maxChoices + ", choices: " + choices)
    choices.size match {
      case x if x < minChoices => true
      case x if x > maxChoices => true
      case _ => false
    }
  }

  def buildDescription(purpose: Verb, minChoices: Int, maxChoices: Int) : SelfDescribing = {
    if (minChoices == maxChoices && minChoices > 0)
      description("Choose", minChoices, purpose)
    else if (minChoices == 0)
      description("Choose up to", maxChoices, purpose)
    else
      error("don't know how to build this description")
  }

  def description(prefix: String, choiceCount: Int, purpose: Verb) : SelfDescribing = {
    QuickDescription(prefix + " " + pluralise(choiceCount) + " to " + purpose.present)
  }

  def pluralise(num: Int) : String = num match {
    case 1 => "1 card"
    case a => a.toString + " cards"
  }

  

  def describe() : String = {
    "player named " + name
  }

}

case class QuickDescription(val desc: String) extends SelfDescribing {
  def describe() : String = { desc }
}

