package org.grumpysoft.actioncards

import org.grumpysoft._

object ThiefSpec extends ActionCardSpecBase {

  val stacks = Stacks.empty()

  val playerTwoStacks = Stacks.deckOnly(witchAndDuchy)
  val playerThreeStacks = Stacks.deckOnly(copperAndSilver)
  val playerFourStacks = Stacks.deckOnly(copperDuchyAndEstate)

  val stolenSilver = copperAndSilver.drop(1)

  val table = makeTable(playerTwoStacks, playerThreeStacks, playerFourStacks)
  "thief" should {
    // force player three to trash a silver, then pick it up
    playerOne.chooseFrom(copperAndSilver, ThiefTrash, 1, 1) returns stolenSilver
    playerOne.chooseFrom(stolenSilver, Gain, 0, 1) returns stolenSilver

    // player four must discard a copper, but we don't pick it up
    playerOne.chooseFrom(copperDuchyAndEstate.take(1), Gain, 0, 1) returns List()

    val actionResult = new Thief().play(stacks, playerOne, supply, table)
    "add stolen cards to the player's discard" in {
      actionResult.stacks.discard must_==stolenSilver
    }
    "discard any cards revealed that weren't treasure cards" in {
      actionResult.table.head._1.discard must_==witchAndDuchy
      actionResult.table.drop(1).head._1.discard must_==copperAndSilver.take(1)
      actionResult.table.last._1.discard must_==copperDuchyAndEstate.tail.take(1)
    }
    "transmit the discard events correctly" in {
      checkEventReceived(playerOne, Gain, stolenSilver, table.map(_._2))
      checkEventReceived(playerTwo, Discard, witchAndDuchy, List(playerOne ,playerThree, playerFour))
      checkEventReceived(playerThree, Discard, copperAndSilver.take(1), List(playerOne, playerTwo, playerFour))
      checkEventReceived(playerThree, Trash, stolenSilver, List(playerOne, playerTwo, playerFour))
      checkEventReceived(playerFour, Trash, copperDuchyAndEstate.take(1), playerOne :: table.map(_._2).toList.take(2))
      checkEventReceived(playerFour, Discard, copperDuchyAndEstate.tail.take(1), playerOne :: table.map(_._2).toList.take(2))
    }
  }

}

case class ThiefResult(stacks: Stacks, card: Option[Card]) {}

object Thieve {
  case class ThiefImpl(thief: GenericPlayer[Card], victim: GenericPlayer[Card], table: Seq[GenericPlayer[Card]]) {
    lazy val nonVictims = thief :: allBut(victim, table)

    private def allBut(player: GenericPlayer[Card], table: Seq[GenericPlayer[Card]]) : List[GenericPlayer[Card]] = {
      table.filter(_.ne(player)).toList
    }

    private def isTreasureCard(card: Card) : Boolean = card match {
      case tc: TreasureCard => true
      case _ => false
    }

    private def steal(stacks: Stacks) : (List[Card], List[Card]) = {
      stacks.deck.take(2).partition(isTreasureCard(_))
    }

    private def doDiscard(stacks: Stacks, toDiscard: List[Card]): Stacks = {
      if (toDiscard.size > 0) nonVictims.foreach(other => other.playerEvent(victim, Discard, toDiscard))
      Stacks(stacks.deck.drop(2), stacks.hand, toDiscard ++ stacks.discard)
    }

    private def trashAndQuery(card: Seq[Card], targetStacks: Stacks) : ThiefResult = {
      nonVictims.foreach(_.playerEvent(victim, Trash, card))
      val gain = thief.chooseFrom(card, Gain, 0, 1)
      if (gain.size > 0) table.foreach(_.playerEvent(thief, Gain, gain))
      ThiefResult(targetStacks, gain.headOption)
    }

    private def pickTrashThenQuery(cards: Seq[Card], targetStacks: Stacks) : ThiefResult = {
      val toTrash = thief.chooseFrom(cards, ThiefTrash, 1, 1)
      val toDiscard = cards.filter(_.ne(toTrash.head))
      nonVictims.foreach(_.playerEvent(victim, Discard, toDiscard))
      trashAndQuery(toTrash, targetStacks.gain(toDiscard))
    }

    private def doThieving(stacks: Stacks, treasure: Seq[Card]) : ThiefResult = treasure.size match {
      case 0 => ThiefResult(stacks, None)
      case 1 => trashAndQuery(treasure, stacks)
      case 2 => pickTrashThenQuery(treasure, stacks)
    }

    def thieveFrom(stacks: Stacks) : ThiefResult = {
      val (treasure, discard) = steal(stacks)
      val afterDiscard = doDiscard(stacks, discard)
      doThieving(afterDiscard, treasure)
    }

  }

  def apply(thief: GenericPlayer[Card], victim: GenericPlayer[Card], targetStacks: Stacks, table: Seq[GenericPlayer[Card]]) : ThiefResult = {
    new ThiefImpl(thief, victim, table).thieveFrom(targetStacks)
  }
}

//case class Thieve(thief: GenericPlayer[Card], victim: GenericPlayer[Card], table: Seq[GenericPlayer[Card]]) {}

class Thief extends ActionCard(4) {

  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    val thiefResults = table.map(a => Thieve(player, a._2, a._1, table.map(_._2)))
    val gainedCards = thiefResults.map(_.card).filter(_.isDefined).map(_.get)
    ActionResult.noTreasureOrBuys(stacks.gain(gainedCards), supply, thiefResults.map(_.stacks).zip(table.map(_._2)))
  }

  def describe() = "Thief"
}