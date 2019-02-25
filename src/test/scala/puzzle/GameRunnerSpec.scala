package puzzle

import org.scalatest.WordSpec

class GameRunnerSpec extends WordSpec {

  private final val n = 4

  "isSolvable" should {

    "be solvable" in {

      val state = State(n, None, 13, Seq(
        1, 2, 3, 4,
        5, 6, 7, 8,
        9, 10, 11, 12,
        13, NoElement, 14, 15
      ))

      assert(GameRunner.isSolvable(state))
    }

    "not be solvable" in {
      val state = State(n, None, 15, Seq(
        1, 2, 3, 4,
        5, 6, 7, 8,
        9, 10, 11, 12,
        13, 15, 14, NoElement
      ))

      assert(!GameRunner.isSolvable(state))
    }
  }

  "genSolvable" should {

    "always be solvable" in {

      for (n <- 2 to 8) {
        val solution = GameRunner.getSolution(n)

        for (_ <- 0 until 1000) {
          val state = GameRunner.genSolvable(solution)
          assert(GameRunner.isSolvable(state))
        }
      }

    }
  }
}
