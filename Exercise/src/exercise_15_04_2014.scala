/**
 * Created by sacry on 15/04/14.
 */

object FizzBuzz {
  def withCase() {
    for (i <- (1 to 100)) {
      println(i match {
        case i if i % 3 == 0 => "fizz"
        case i if i % 5 == 0 => "buzz"
        case _ => i
      })
    }
  }

  def withFold() {
    println((0 to 99).foldLeft("") {
      case (acc, i) => {
        val tmp = "Fizz" * ((i % 3) / 2) + "Buzz" * ((i % 5) / 4)
        acc + (if (tmp.isEmpty) i + 1 else tmp) + "\n"
      }})
  }


  def withoutCase() {
    for (i <- (0 to 99)) {
      val tmp = "Fizz" * ((i % 3) / 2) + "Buzz" * ((i % 5) / 4)
      println(if (tmp.isEmpty) i + 1 else tmp)
    }
  }

  def main(args: Array[String]) {
    FizzBuzz.withCase()
    FizzBuzz.withoutCase()
    FizzBuzz.withFold()
  }
}

sealed trait Tree

case class Node(left: Tree, right: Tree) extends Tree

case class Leaf(value: Int) extends Tree

object TreeTest {

  def inorderTreeWalk(tree: Tree): List[Int] = tree match {
    case x: Leaf => List(x.value)
    case x: Node => inorderTreeWalk(x.left) ++ inorderTreeWalk(x.right)
  }

  def treeMap(tree: Tree)(f: (Int => Int)): Tree = tree match {
    case x: Leaf => Leaf(f(x.value))
    case x: Node => Node(treeMap(x.left)(f), treeMap(x.right)(f))
  }

  def main(args: Array[String]) {
    val tree = Node(
      Node(
        Leaf(1),
        Node(
          Leaf(1),
          Leaf(2)
        )
      ),
      Node(
        Leaf(5),
        Leaf(5)
      )
    )
    println(inorderTreeWalk(tree))
    println(inorderTreeWalk(treeMap(tree)(_ + 10)))
  }
}
