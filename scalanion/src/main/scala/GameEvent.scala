package org.grumpysoft

sealed abstract class GameEvent

case class Start(startTime: Long) extends GameEvent
case class End(leaderBoard: Seq[(SelfDescribing, Int)]) extends GameEvent