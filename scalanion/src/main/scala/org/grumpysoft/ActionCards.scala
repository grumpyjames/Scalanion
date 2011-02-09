package org.grumpysoft

import org.grumpysoft.TreasureCards.Silver
import collection.immutable.List

object ActionCards {

  def isVictoryCard(card: Card) : Boolean = {
    card match {
      case vc: VictoryCard => true
      case _ => false
    }
  }

  object Bureaucrat {
    def apply() : Bureaucrat = { new Bureaucrat }
  }


  class Bureaucrat extends ActionCard(4) {

    type stacksWithPlayer = Tuple2[Stacks, GenericPlayer[Card]]
    val oneSilver: List[Silver] = List(Silver())

    // TODO: must this return a Tuple?
    def addRedTape(stacks: Stacks, player: GenericPlayer[Card], victoryCards: List[Card])
      : (Stacks, Card) = {
      val toReplace = player.chooseFrom(victoryCards, PlaceOnDeck, 1, 1)
      (stacks.replace(toReplace), toReplace.head)
    }

    def attack(stacksAndPlayer: stacksWithPlayer, otherPlayers: Seq[GenericPlayer[Card]]) : stacksWithPlayer = {
      val (stacks, player) = stacksAndPlayer
      val victoryCards = stacks.hand.filter(isVictoryCard(_))
      if (victoryCards.size > 0) {
        val (newStacks, replaced) = addRedTape(stacks, player, victoryCards)
        otherPlayers.map(_.playerEvent(player, PlaceOnDeck, List(replaced)))
        return (newStacks, player)
      } else {
        otherPlayers.map(_.playerEvent(player, RevealHand, stacks.hand))
        stacksAndPlayer
      }
    }

    def performAttack(table: Table, player: GenericPlayer[Card]) : Table = {
      table.map(a => attack(a, otherPlayers(player, table, a)))
    }

    def otherPlayers(player: GenericPlayer[Card], table: Table, a: stacksWithPlayer) : List[GenericPlayer[Card]] = {
      player :: table.filter(_.ne(a)).map(_._2).toList
    }

    def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
      table.map(_._2.playerEvent(player, PlaceOnDeck, oneSilver))
      ActionResult(0, Stacks(oneSilver ++ stacks.deck, stacks.hand, stacks.discard), supply.buy(Silver()), performAttack(table, player))
    }
    def describe() = { "Bureaucrat" }
  }

  object Chancellor {
    def apply() : Chancellor = { new Chancellor }
  }

  class Chancellor extends ActionCard(3) {
    def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
      player.query(DiscardYourDeck) match {
        case true => {
          table.map(_._2.playerEvent(player, DeckDiscard, stacks.deck))
          ActionResult(2, Stacks(List(), stacks.hand, stacks.discard ++ stacks.deck), supply, table)
        }
        case false => ActionResult(2, stacks, supply, table)
      }
    }

    def describe() : String = {
      "Chancellor"
    }
  }

  object Chapel {
    def apply() : Chapel = { new Chapel }
  }

  def anyEqTo(cards: Seq[Card], candidate: Card): Boolean = {
    !cards.find(b => b.eq(candidate)).isDefined
  }

  class Chapel extends ActionCard(2) {
    def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
      val toTrash = player.chooseFrom(stacks.hand, Trash, 0, 4)
      table.map(_._2.playerEvent(player, Trash, toTrash))
      ActionResult(0, Stacks(stacks.deck, stacks.hand.filter(anyEqTo(toTrash, _)), stacks.discard), supply, table)
    }
    def describe() : String = { "Chapel" }
  }

  object Remodel {
    def apply() : Remodel = {
      new Remodel
    }
  }

  class Remodel extends ActionCard(4) {
    def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = { ActionResult(0, Stacks.base(), supply, table) }
    def describe() : String = { "Remodel" }
  }

  object Witch {
    def apply() = { new Witch }
  }

  class Witch extends ActionCard(5) {
    def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = { ActionResult(0, Stacks.base(), supply, table) }
    def describe() : String = { "Witch" }
  }

  object Militia {
    def apply() = { new Militia }
  }

  class Militia extends ActionCard(4) {
    def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = { ActionResult(0, Stacks.base(), supply, table) }
    def describe() : String = { "Militia" }
  }

}
