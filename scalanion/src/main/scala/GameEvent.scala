package org.grumpysoft

sealed abstract class GameEvent

case class Start(startTime: Long) extends org.grumpysoft.GameEvent
case class End(leaderBoard: Seq[(SelfDescribing, Int)]) extends org.grumpysoft.GameEvent