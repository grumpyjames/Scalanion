package org.grumpysoft

object ActionCards {

  object Chapel {
    def apply() : Chapel = { new Chapel }
  }

  class Chapel extends ActionCard(2) {
    def play(stacks: Stacks, player: GenericPlayer[Card]) : Stacks = {
      val toTrash = player.chooseFrom(stacks.hand, Trash, 0, 4)
      // want object equality here.
      return Stacks(stacks.deck, stacks.hand.filter(a => !toTrash.find(b => b.eq(a)).isDefined), stacks.discard)
    }
    def describe() : String = { "Chapel" }
  }

  object Remodel {
    def apply() : Remodel = {
      new Remodel
    }
  }

  class Remodel extends ActionCard(4) {
    def play(stacks: Stacks, player: GenericPlayer[Card]) : Stacks = { Stacks.base() }
    def describe() : String = { "Remodel" }
  }

  object Witch {
    def apply() = { new Witch }
  }

  class Witch extends ActionCard(5) {
    def play(stacks: Stacks, player: GenericPlayer[Card]) : Stacks = { Stacks.base() }
    def describe() : String = { "Witch" }
  }

  object Militia {
    def apply() = { new Militia }
  }

  class Militia extends ActionCard(4) {
    def play(stacks: Stacks, player: GenericPlayer[Card]) : Stacks = { Stacks.base() }
    def describe() : String = { "Militia" }
  }

}
