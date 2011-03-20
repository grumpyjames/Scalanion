package org.grumpysoft

import org.specs.Specification

import TreasureCards._
import VictoryCards._
import actioncards.Remodel

import org.specs.matcher.Matcher

object SimpleSupplySpec extends Specification {

  val coppers = List(Copper(), Copper())
  val silvers = List(Silver(), Silver())
  val oneGold = List(Gold())

  "simple supply" should {
    "return a new supply with a reduced pile of the card bought" in {
      val supply = SimpleSupply(List(coppers, silvers, oneGold))
      val newSupply = supply.buy(Gold())
      newSupply.availableCards(11) must_==List(Copper(),Silver())
    }

    "work the same for action cards" in {
      val supply = SimpleSupply(List(Remodel().times(10)))
      val newSupply = supply.buy(Remodel())
      newSupply.availableCards(4) must_==List(Remodel())
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
      supply.availableCards(1) must_==List(Copper(), Curse())
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

  case class beSupplyContaining(count: Int, card: Card) extends Matcher[Supply] {
    def apply(a: => Supply) = {
      (a.available(card) && !depleted(a).available(card), "matched", "did not match")
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
