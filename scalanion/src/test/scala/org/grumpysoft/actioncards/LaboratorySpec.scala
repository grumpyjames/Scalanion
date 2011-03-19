package org.grumpysoft.actioncards

import org.grumpysoft._

object LaboratorySpec extends ActionCardSpecBase {

  val playerOneStacks = Stacks(estateAndDuchy, copperDuchyAndEstate, List())

  "laboratory" should {
    "add an action and two cards" in {
      val actionResult = Laboratory().play(playerOneStacks, playerOne, supply, eventOnlyTable)
      actionResult.actions must_==1
      actionResult.stacks.hand must_==estateAndDuchy ++ copperDuchyAndEstate
    }
  }

}

object Laboratory {
 def apply() = new Laboratory()
}

class Laboratory extends ActionCard(5) {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionResult.noTreasureOrBuys(1, stacks.addCards(2), supply, table)
  }

  def describe() = "Laboratory"

  protected def copyThyself() = Laboratory()
}
