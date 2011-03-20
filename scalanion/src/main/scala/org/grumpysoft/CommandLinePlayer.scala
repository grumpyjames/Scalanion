package org.grumpysoft

import java.io.{InputStreamReader, BufferedReader}

object CommandLinePlayer {
  def forStdInAndOut(name: String) = {
    new CommandLinePlayer(name, new FormattedPrompter(new StreamedUserInput(new BufferedReader(new InputStreamReader(System.in))), StdOutPrinter, new OptionFormatter))
  }

  private case object StdOutPrinter extends Printer {
    def println(s: String) = System.out.println(s)
  }
}

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
  
  def playerEvent(player: SelfDescribing, action: Verb, cards: Seq[Card]) : Unit = {
    if (!(player eq this) && !cards.isEmpty)
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

  private val noAndYes = List(QuickDescription("No"), QuickDescription("Yes"))
  private val invalidQueryResponse: Seq[Int] => Boolean = result => result.size != 1 && !List(0, 1).contains(result.head)


  def doQuery(text: String): Boolean = {
    nextInput(QuickDescription(text), noAndYes).dropWhile(invalidQueryResponse).head.head match {
      case 0 => false
      case _ => true
    }
  }

  def query(question: Query) : Boolean = question match {
    case BasicQuestion(text) => doQuery(text)
    case ChooseForOtherPlayer(cards, otherPlayer, verb) => {
      cards.isEmpty match {
        case true => true
        case false => doQuery("Should " + otherPlayer.describe + " " + verb.present + " " + cards.map(_.describe).reduceLeft(_ + ", " + _) + "?")
      }
    }
  }

  def gameEvent(event: GameEvent) = event match {
    case Start(startTime) => userInterface.prompt(QuickDescription("The game begins!"))
    case End(leaderboard) => showLeaderboard(leaderboard)
  }

  private def showLeaderboard(leaderboard: Seq[(SelfDescribing, Int)]) : Unit = {
    userInterface.prompt(QuickDescription("The game ended. Scores were:"))
    leaderboard.indices.zip(leaderboard).foreach(indexAndPlayerScore =>
      userInterface.prompt(QuickDescription(String.valueOf(indexAndPlayerScore._1 + 1)
        + ".) " + indexAndPlayerScore._2._1.describe + " : " + String.valueOf(indexAndPlayerScore._2._2)))
    )
  }
}

private case class QuickDescription(val desc: String) extends SelfDescribing {
  def describe() : String = { desc }
}

