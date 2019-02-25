package puzzle

trait Game {

  def beforeStart(): Unit

  def winGame(state: State): Unit
  def looseGame(state: State): Unit

  def initialState(): State
  def isSolution(state: State): Boolean

  def nextUserAction(currentState: State): Option[State]

  final def run(): Unit = {
    beforeStart()

    var state = initialState()
    var isLoosed = false

    while (!isSolution(state) && !isLoosed) {
      nextUserAction(state) match {
        case Some(newState) =>
          state = newState
        case None =>
          isLoosed = true
      }
    }

    if (isLoosed) {
      looseGame(state)
    } else {
      winGame(state)
    }

  }

}