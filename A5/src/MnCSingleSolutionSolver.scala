/**
 * Created by sacry on 21/04/14.
 */

sealed trait TState {
  def m: Int

  def c: Int

  def b: Int

  override def toString = "(" + m + ", " + c + ", " + b + ")"
}

case class State(m: Int, c: Int, b: Int) extends TState {

  lazy val right = State(3 - m, 3 - c, 1 - b)

  def isValid = m <= 3 && c <= 3 && b <= 1 && List(m, c, b).forall(_ >= 0) && !invalid && !right.invalid

  def invalid = (m > 0) && (m < c)

  def hasNoMoreActions = (m == 0) && (c == 0)

  override def toString = "State" + super.toString
}

case class Action(m: Int, c: Int, b: Int) extends TState {

  lazy val inverse = Action(-m, -c, -b)

  def add(wss: State) = State(wss.m + m, wss.c + c, wss.b + b)

  override def toString = "Action" + super.toString
}

object MissionAndCannibals {

  val leftToRight = List(
    Action(2, 0, 1), Action(1, 0, 1),
    Action(1, 1, 1), Action(0, 1, 1),
    Action(0, 2, 1)
  )

  val stateActions = leftToRight ++ leftToRight.map(_.inverse)

  def solve(currentState: State, steps: List[Action], seen: List[State]): List[Action] = {
    for (stateAction <- stateActions) {
      val newState = stateAction.add(currentState)
      if (newState.isValid) {
        if (newState.hasNoMoreActions)
          return stateAction :: steps
        if (!seen.contains(newState)) {
          val solved = solve(newState, stateAction :: steps, newState :: seen)
          if (solved.size > 0)
            return solved
        }
      }
    }
    List()
  }

  def printPath(initialState: State, stateList: List[Action], count: Int = 1) {
    if (!stateList.isEmpty) {
      val stateAction = stateList.head
      val newState = stateAction.add(initialState)
      val show = count + ".\t" + stateAction + ",\tLeft: " + newState + ",\tRight: " + newState.right
      println(show)
      printPath(newState, stateList.tail, count + 1)
    }
  }

  def main(args: Array[String]) {
    val initialState = State(3, 3, 1)
    val res = solve(initialState, List(), List(initialState))
    printPath(initialState, res)
  }
}
