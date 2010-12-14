package org.grumpysoft

trait Card extends SelfDescribing {
  def cost() : Int;
}

trait ActionCard extends Card {
  def play() : Unit;
}

trait TreasureCard extends Card {
  def value() : Int;
}

trait VictoryCard extends Card {
  def victoryPoints() : Int;
}
