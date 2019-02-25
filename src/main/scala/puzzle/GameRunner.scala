package puzzle

import scala.io.StdIn
import scala.util.{Random, Try}


object GameRunner {

  def compareTo(state: State, other: State): Int =
    (state.elements zip other.elements)
      .count { case (s1, s2) => s1 != s2 }


  def getSolution(n: Int): State =
    State(n, None, n * n - 1, (1 until n * n) :+ NoElement)


  def genSolvable(solution: State): State = {
    // every unsolvable puzzle is paired with a unique solvable puzzle that differs only in the first two tiles or last two tiles
    // if the first 2 tiles contain empty tile then swap last two tiles
    // https://www.sitepoint.com/randomizing-sliding-puzzle-tiles

    val shuffledElements: Seq[Int] = Random.shuffle(solution.elements)
    val emptyIndex = shuffledElements.zipWithIndex
      .find { case (e, _) => e == NoElement }
      .map { case (_, i) => i }
      .get

    val state = State(solution.n, None, emptyIndex, shuffledElements)

    def swapTiles(tiles: Seq[Int], i: Int, j: Int) = {
      val eI = tiles(i)
      tiles.updated(i, tiles(j)).updated(j, eI)
    }

    val randomizedElements =
      if (!isSolvable(state)) {
        val len = shuffledElements.length

        if (emptyIndex < 2) {
          swapTiles(shuffledElements, len - 2, len - 1)
        } else {
          swapTiles(shuffledElements, 0, 1)
        }
      } else {
        shuffledElements
      }

    state.copy(elements = randomizedElements)
  }


  def isSolvable(state: State): Boolean = {

    val elements: Array[Int] = state.elements.toArray
    val numOfInversions = {
      for {
        i <- elements.indices if elements(i) != NoElement
        j <- i until elements.length if elements(j) != NoElement && elements(i) > elements(j)
      } yield 1
    }.size

    // https://www.cs.bham.ac.uk/~mdr/teaching/modules04/java2/TilesSolvability.html
    if (state.n % 2 == 0) {

      // odd row from last
      if (((state.n * state.n - 1 - state.empty) / state.n) % 2 == 0) {
        numOfInversions % 2 == 0
      } else {
        // even row from last
        numOfInversions % 2 == 1
      }
    } else {
      numOfInversions % 2 == 0
    }
  }


  def main(args: Array[String]): Unit = {

    // direction of the tile move, not where the empty tile is moved (it's vice versa)
    val operationIndex: Map[String, Move] = Map(
      "w" -> Down,
      "a" -> Right,
      "d" -> Left,
      "s" -> Up
    )

    val game: Game = new Game {

      private var n: Int = _
      private var solution: State = _

      override def beforeStart(): Unit = {

        println(
          s"""
             |* * * * * * * * * * * * *
             |* STARTING NEW GAME
             |* * * * * * * * * * * * *
          """.stripMargin)

        val defaultN = 4
        val minN = 2
        val maxN = 5 // no reason to allow more, too complex =D

        n = Try {
          StdIn.readLine(s"Please, input size of board [$minN <= N <= $maxN] N (default $defaultN): ").toInt
        }
          .toOption
          .filter(n => n >= minN && n <= maxN)
          .getOrElse(defaultN)

        solution = getSolution(n)
      }

      override def winGame(winState: State): Unit =
        println(
          s"""
            |* * * * * * * * * * * * *
            |* SOLVED SOLVED SOLVED !!
            |* * * * * * * * * * * * *
            |$winState
            |* * * * * * * * * * * *
          """.stripMargin)

      override def looseGame(looseState: State): Unit =
        println(
          """
            |* * * * * * * * * * * * *
            |* DON'T GIVE UP NEXT TIME
            |* * * * * * * * * * * * *
          """.stripMargin)

      override def initialState(): State =
        genSolvable(solution)

      override def nextUserAction(state: State): Option[State] = {
        println(
          """
            |
            |Current state
          """.stripMargin)
        println(state)

        val possibleStates: Map[String, (Move, State)] = operationIndex
          .mapValues { op => op -> op.tryMove(state.n, state) }
          .filter { case (_, (_, opTry)) => opTry.isDefined }
          .mapValues { case (op, opTry) => op -> opTry.get }

        possibleStates.foreach {
          case (k, (mv, _)) =>
            println(s"$k - move $mv")
        }


        val quitKey = "q"

        println(s"$quitKey - Give up")
        var line = ""
        do {
          line = StdIn.readLine("Please, make an action: ")

        } while (line != quitKey && !possibleStates.contains(line))

        if (line == quitKey) {
          None
        } else {
          val (_, nextState) = possibleStates(line)
          Some(nextState)
        }
      }

      override def isSolution(state: State): Boolean =
        compareTo(state, solution) == 0

    }

    game.run()
  }
}