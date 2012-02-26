package org.grumpysoft

import org.specs2.mutable.Specification
import org.grumpysoft.VictoryCards._
import org.grumpysoft.TreasureCards._

object LeaderboardSpec extends Specification {
  import Scorer._

  "a game" should {
    val cardsScoringFive = List(Province(), Curse(), Copper())
    val cardsScoringSix = List(Duchy(), Duchy())
    val cardsScoringTen = List(Province(), Silver(), Gold(), Duchy(), Estate())

    val stacksScoringTwentyOne = Stacks(cardsScoringFive, cardsScoringTen, cardsScoringSix)
    val stacksScoringTwelve = Stacks(cardsScoringSix, Nil, cardsScoringSix)
    val stacksScoringTwentySix = Stacks(cardsScoringTen, cardsScoringSix, cardsScoringTen)

    val associatedPlayers = List.fill(3)(new SinkPlayer)
    val unorderedStacks = List(stacksScoringTwelve, stacksScoringTwentyOne, stacksScoringTwentySix)

    val expectedLeaderboard = associatedPlayers.reverse.zip(List(26, 21, 12))

    "return the leaderboard sorted in order of highest score" in {
      val table = associatedPlayers zip unorderedStacks
      leaderboard(table) must_==expectedLeaderboard
    }
  }

}