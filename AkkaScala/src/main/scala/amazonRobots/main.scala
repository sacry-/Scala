package amazonRobots

import akka.actor.{ActorRef, Actor}
import Protocol._
import amazonRobots.Grid.TGrid

/**
 * Created by sacry on 16/06/14.
 */

class Robot extends Actor {


  override def receive: Receive = {
    case Position(x, y) =>
      ???
  }
}


sealed trait GridElement

sealed trait Accessible

case class EmptyGrid(p: Position) extends GridElement with Accessible {
  override def toString = "0"
}

case class WareGrid(p: Position) extends GridElement {
  override def toString = "1"
}

case class PackGrid(p: Position) extends GridElement {
  override def toString = "2"
}

case class OccupiedGrid(p: Position, actor: ActorRef) extends GridElement {
  override def toString = "3"
}


case class Grid(positions: String) {

  val grid: TGrid = fromStringToGrid(positions)

  override def toString = ("-" * 40) + "\n" + grid
    .map(_
    .map(_.toString).mkString(" ")
    ).mkString("\n")


  def fromStringToGrid(s: String): TGrid = {
    val rows: Array[String] = s.split(",")
    def f(i: Int, j: Int, c: Char): GridElement = c match {
      case '0' => EmptyGrid(Position(i, j))
      case '1' => WareGrid(Position(i, j))
      case '2' => PackGrid(Position(i, j))
    }
    rows.zipWithIndex.map(z => z._1.zipWithIndex.map(t => f(z._2, t._2, t._1)).toArray)
  }
}

object Grid extends App {
  type TGrid = Array[Array[GridElement]]
  val positions = "1110011,0011001,0011001,1011001,1011001,0000000,0022200"
  val numRobots = 6
  val orderMaxSize = 50
  println(Grid(positions))
}

case class Order(articles: List[Article]) {
  val size = articles.map(_.size).sum
}

case class Article(size: Int, name: String, p: Position)

object main extends App {

}
