
object MnCGenericAll {
  val (start, max_boat, goal) = ((3, 3, 0), 2, (0, 0, 1))
  val (left, right) = (0, 1)
  val valid_actions = (for (i <- 0 to max_boat; j <- 0 to max_boat if (i + j <= max_boat)) yield (i, j)).toList.drop(1)

  type Action = Pair[Int, Int]
  type State = Triple[Int, Int, Int]
  type Path = List[State]

  def isValid(state: State): Boolean = (state, start) match {
    case ((m, c, _), (m_, c_, _)) if (m >= 0) && (m <= m_) && (c >= 0) && (c <= c_) =>
      !((c > m) && (m > 0) || (c_ - c > (m_ - m) && (m_ - m > 0)))
    case _ => false
  }

  def change(state: State)(action: Action) = (state, action) match {
    case ((m, c, b), (md, cd)) => if (b == left) (m - md, c - cd, right) else (m + md, c + cd, left)
  }

  def possible_states(state: State): Path =
    for ((x, y) <- valid_actions; val action = change(state)(x, y); if isValid(action)) yield action

  def consecutiveStep(stream_elem: List[Path]): List[Path] = {
    for (path <- stream_elem; action <- possible_states(path.head) if (path.head != goal && !path.contains(action)))
    yield action :: path
  }

  def getActions(path: Path): Path = path.zip(path.tail).map {
    case ((m, c, b), (m_, c_, _)) => (m_ - m, c_ - c, b)
  }

  def extractPath(reversedPath: Path): Path = reversedPath.dropWhile(overflowed => overflowed != goal).reverse

  def solve = Stream.iterate(List[Path](List(start)))(consecutiveStep)
    .takeWhile(!_.isEmpty)
    .flatten
    .map(extractPath)
    .dropWhile(_.isEmpty)

  def main(args: Array[String]) {
    solve.map(getActions).foreach(println)
  }
}