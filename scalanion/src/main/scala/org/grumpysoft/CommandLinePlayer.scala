package org.grumpysoft

class CommandLinePlayer(private val name: String, private val userInterface: Promptable) extends Player {

  def describe() : String = {
    "The player " + name
  }

  def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[Int] = {
    nextInput(buildDescription(purpose, minChoices, maxChoices), cards).dropWhile(invalid(minChoices, maxChoices, _)).head
  }

  def newHand(hand: Seq[Card]) : Unit = {
    userInterface.prompt(QuickDescription("Your hand now contains: " + hand.map(_.describe).reduceLeft(_ + ", " + _)))
  }
  
  def playerEvent(player: GenericPlayer[Any], action: Verb, cards: Seq[Card]) : Unit = {
    if (!(player eq this)) 
      userInterface.prompt(QuickDescription(player.describe() + " " + action.past + ": " + cards.map(_.describe).reduceLeft(_ + ", " + _)))
  }

  private def nextInput(greeting: SelfDescribing, options: Seq[SelfDescribing]) : Stream[Seq[Int]] = {
    Stream.cons(userInterface.prompt(greeting, options), nextInput(greeting, options))
  }

  private def invalid(minChoices: Int, maxChoices: Int, choices: Seq[Int]) : Boolean = {
    choices.size match {
      case x if x < minChoices => true
      case x if x > maxChoices => true
      case _ => false
    }
  }

  private def buildDescription(purpose: Verb, minChoices: Int, maxChoices: Int) : SelfDescribing = {
    if (minChoices == maxChoices && minChoices > 0)
      description("Choose", minChoices, purpose)
    else if (minChoices == 0)
      description("Choose up to", maxChoices, purpose)
    else
      error("don't know how to build this description")
  }

  private def description(prefix: String, choiceCount: Int, purpose: Verb) : SelfDescribing = {
    QuickDescription(prefix + " " + pluralise(choiceCount) + " to " + purpose.present)
  }

  private def pluralise(num: Int) : String = num match {
    case 1 => "1 card"
    case a => a.toString + " cards"
  }

  def query(question: Query) : Boolean = {
    //TODO: implement
    true
  }
}

private case class QuickDescription(val desc: String) extends SelfDescribing {
  def describe() : String = { desc }
}

