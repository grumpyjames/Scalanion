package org.grumpysoft

import org.specs2.mutable.Specification

import Card._
import TreasureCards._
import VictoryCards._
import actioncards._
import org.specs2.matcher.{MatchResult, Expectable}

object SimpleSupplySpec extends Specification {

  val coppers = List(Copper(), Copper())
  val silvers = List(Silver(), Silver())
  val oneGold = List(Gold())

  "simple supply" should {
    "return a new supply with a reduced pile of the card bought" in {
      val supply = SimpleSupply(List(coppers, silvers, oneGold))
      val newSupply = supply.buy(Gold())
      newSupply.availableCards(11) must_==List(Silver(), Copper())
    }

    "work the same for action cards" in {
      val supply = SimpleSupply(List(Remodel().times(10), Spy().times(1)))
      val newSupply = supply.buy(Remodel())
      newSupply.availableCards(4) must_==List(Remodel().toActionCard, Spy().toActionCard).reverse
    }

    "know itself" in {
      val supply = SimpleSupply(List(coppers, silvers))
      supply.available(Copper()) must_==true
      supply.available(Silver()) must_==true
      supply.available(Gold()) must_==false
    }

    "know when the game is over" in {
      val supply = Supplies.forTwo
      buy(8, Province(), supply).gameOver must_==true
    }

    "only list cards available for the treasure cost provided or less" in {
      val supply = Supplies.forTwo
      supply.availableCards(1) must_==List(Curse(), Copper())
    }

    "sort available cards by price" in {
      val supply = SimpleSupply(List(Gold().times(3), Province().times(3), Copper().times(3), Remodel().times(4)))
      supply.availableCards(10) must_==List(Province(), Gold(), Remodel().toActionCard, Copper())
    }
  }

  def buy(count: Int, card: Card, supply: Supply) : Supply = {
    var result = supply
    Range(0, count).foreach{ a => result = result.buy(card) }
    result
  }

  def contain(count: Int, card: Card)(implicit supply: Supply) : Unit = {
    supply.available(card) must_==true
    val depletedSupply = buy(count, card, supply)
    depletedSupply.available(card) must_==false
  }

  case class beSupplyContaining(count: Int, card: Card) extends org.specs2.matcher.Matcher[Supply] {
    /**
     * apply this matcher to an Expectable
     * @return a MatchResult describing the outcome of the match
     */
    def apply[S <: Supply](a: Expectable[S]): MatchResult[S] = {
      result(a.value.available(card) && !depleted(a.value).available(card), "matched", "did not match", a)
    }

    def depleted(a : Supply) : Supply = {
      buy(count, card, a)
    }
  }

  "two player supply" should {
    "contain eight provinces" in {
      val supply = Supplies.forTwo()
      supply must beSupplyContaining(8, Province())
    }
  }
      

}
