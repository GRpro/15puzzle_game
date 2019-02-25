
package object puzzle {

  final val NoElement = -1


  case class State(n: Int, prev: Option[State], empty: Int, elements: Seq[Int]) {
    override def toString: String = {
      var res: String = ""
      for ((e, i) <- elements.zipWithIndex) {

        if (i % n == 0) {
          res += "\n"
        }

        if (e == NoElement) {
          res += s"\t*"
        } else {
          res += s"\t$e"
        }

      }
      res + "\n"
    }
  }


  /**
    * Represents operation of moving the empty tile
    */
  sealed trait Move {
    def tryMove(n: Int, state: State): Option[State]
  }



  case object Left extends Move {
    override def tryMove(n: Int, state: State): Option[State] = {

      /*
          * 1 2
          * 4 5
          * 7 8
      */
      def canMoveLeft(i: Int): Boolean =
        i % n != 0

      def moveLeft(i: Int): State = {
        val newEmptyInd = i - 1
        val e = state.elements(newEmptyInd)
        val updatedElements = state.elements.updated(newEmptyInd, NoElement).updated(i, e)
        State(state.n, Some(state), newEmptyInd, updatedElements)
      }

      if (canMoveLeft(state.empty)) Some(moveLeft(state.empty))
      else None
    }

    override def toString: String = "LEFT"
  }

  case object Right extends Move {
    override def tryMove(n: Int, state: State): Option[State] = {
      /*
          0 1 *
          3 4 *
          6 7 *
      */
      def canMoveRight(i: Int): Boolean =
        (i + 1) % n != 0

      def moveRight(i: Int): State = {
        val newEmptyInd = i + 1
        val e = state.elements(newEmptyInd)
        val newElements = state.elements.updated(newEmptyInd, NoElement).updated(i, e)
        State(state.n, Some(state), newEmptyInd, newElements)
      }

      if (canMoveRight(state.empty)) Some(moveRight(state.empty))
      else None
    }

    override def toString: String = "RIGHT"
  }

  case object Up extends Move {
    override def tryMove(n: Int, state: State): Option[State] = {


      /*
          * * *
          3 4 5
          6 7 8
      */
      def canMoveUp(i: Int): Boolean =
        i >= n

      def moveUp(i: Int): State = {
        val newEmptyInd = i - n
        val e = state.elements(newEmptyInd)
        val newState = state.elements.updated(newEmptyInd, NoElement).updated(i, e)
        State(state.n, Some(state), newEmptyInd, newState)
      }

      if (canMoveUp(state.empty)) Some(moveUp(state.empty))
      else None
    }

    override def toString: String = "UP"
  }

  case object Down extends Move {
    override def tryMove(n: Int, state: State): Option[State] = {

      /*
         0 1 2
         3 4 5
         * * *
      */
      def canMoveDown(i: Int): Boolean =
        i < n * (n - 1)

      def moveDown(i: Int): State = {
        val newEmptyInd = i + n
        val e = state.elements(newEmptyInd)
        val newElements = state.elements.updated(newEmptyInd, NoElement).updated(i, e)
        State(state.n, Some(state), newEmptyInd, newElements)
      }

      if (canMoveDown(state.empty)) Some(moveDown(state.empty))
      else None
    }

    override def toString: String = "DOWN"
  }

}
