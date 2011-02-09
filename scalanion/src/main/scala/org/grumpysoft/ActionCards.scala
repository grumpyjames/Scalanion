package org.grumpysoft

object ActionCards {

  object Chancellor {
    def apply() : Chancellor = { new Chancellor }
  }

  class Chancellor extends ActionCard(3) {
    def play(stacks: Stacks, player: GenericPlayer[Card]) : ActionResult = {
      player.query(DiscardYourDeck) match {
        case true => ActionResult(2, Stacks(List(), stacks.hand, stacks.discard ++ stacks.deck))
        case false => ActionResult(2, stacks)
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
    def play(stacks: Stacks, player: GenericPlayer[Card]) : ActionResult = {
      val toTrash = player.chooseFrom(stacks.hand, Trash, 0, 4)
      ActionResult(0, Stacks(stacks.deck, stacks.hand.filter(anyEqTo(toTrash, _)), stacks.discard))
    }
    def describe() : String = { "Chapel" }
  }

  object Remodel {
    def apply() : Remodel = {
      new Remodel
    }
  }

  class Remodel extends ActionCard(4) {
    def play(stacks: Stacks, player: GenericPlayer[Card]) : ActionResult = { ActionResult(0, Stacks.base()) }
    def describe() : String = { "Remodel" }
  }

  object Witch {
    def apply() = { new Witch }
  }

  class Witch extends ActionCard(5) {
    def play(stacks: Stacks, player: GenericPlayer[Card]) : ActionResult = { ActionResult(0, Stacks.base()) }
    def describe() : String = { "Witch" }
  }

  object Militia {
    def apply() = { new Militia }
  }

  class Militia extends ActionCard(4) {
    def play(stacks: Stacks, player: GenericPlayer[Card]) : ActionResult = { ActionResult(0, Stacks.base()) }
    def describe() : String = { "Militia" }
  }

}
