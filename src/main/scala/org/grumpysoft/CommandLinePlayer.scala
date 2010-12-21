package org.grumpysoft

class CommandLinePlayer(private val name: String, private val userInterface: Promptable) extends Player with SelfDescribing {

  def chooseFrom(cards: Seq[Card], purpose: Verb, maxChoices: Int, minChoices: Int) : Seq[Int] = {
    userInterface.prompt(buildDescription(purpose, maxChoices, minChoices),
			 cards)
  }

  def buildDescription(purpose: Verb, maxChoices: Int, minChoices: Int) : SelfDescribing = {
    if (minChoices == maxChoices && minChoices > 0)
      QuickDescription("Choose " + pluralise(minChoices) +  " to " + purpose.present)
    else
      error("don't know how to build this description")
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

