/**
 * Created by Matthias and Swaneet on 23/04/14.
 */

object MissionAndCan {
  val (left, right) = (0, 1)
  val goal = (0, 0, 1)
  val valid_actions = List((1, 0), (0, 1), (1, 1), (2, 0), (0, 2))

  type Action = Pair[Int, Int]
  type State = Triple[Int, Int, Int]
  type Path = List[State]

  def isValid(state: State): Boolean = state match {
    case (m, c, _) if ((m >= 0) && (m <= 3) && (c >= 0) && (c <= 3)) =>
      !(((c > m) && (m > 0)) || ((3 - c > 3 - m) && (3 - m > 0)))
    case _ => false
  }

  def change(state: State)(action: Action) = (state, action) match {
    case ((m, c, b), (md, cd)) if (b == left) => (m - md, c - cd, right)
    case ((m, c, b), (md, cd)) => (m + md, c + cd, left)
  }

  def possible_states(state: State) = (for ((x, y) <- valid_actions) yield change(state)(x, y)).filter(isValid)

  def consecutiveStep(stream_elem: List[Path]): List[Path] = {
    for (path <- stream_elem; action <- possible_states(path.head) if (!path.contains(action)))
    yield action :: path // calculate every possible action and expand the lists if possible
  }

  def solve(state: State) = {
    Stream.iterate(List[Path](List(state)))(consecutiveStep)
      .dropWhile(path => path.head.head != goal) // unwrap first element and drop as long as no goal was reached
      .take(1).head // Stream is lazy so force it to take another step
      .distinct // make paths unique
      .map(_.reverse) // reverse them due to cons (this is faster than appending to a list!)
  }

  def main(args: Array[String]) {
    solve((3, 3, 0)).foreach(println)
  }
}