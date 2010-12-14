package org.grumpysoft

class CommandLinePlayer(private val name: String, private val userInterface: Promptable) extends Player with SelfDescribing {

  def chooseFrom(cards: Seq[Card], purpose: Verb, maxChoices: Int, minChoices: Int) : Seq[Int] = {
    return List()
  }

  def describe() : String = {
    "player named " + name
  }

}
