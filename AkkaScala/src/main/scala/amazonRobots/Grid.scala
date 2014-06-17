package amazonRobots

import amazonRobots.Protocol.Position
import akka.actor.ActorRef

/**
 * Created by sacry on 17/06/14.
 */
sealed trait GridElement

sealed trait Accessible

case class EmptyGrid(p: Position) extends GridElement with Accessible {
  override def toString = "0"
}

case class WareGrid(p: Position, article: Option[Article]) extends GridElement {
  override def toString = "1"
}

case class PackGrid(p: Position) extends GridElement {
  override def toString = "2"
}

case class OccupiedGrid(p: Position, actor: ActorRef) extends GridElement {
  override def toString = "3"
}


case class Grid(positions: String) {

  import amazonRobots.Grid._

  val grid: TGrid = fromStringToGrid(positions)

  override def toString = ("-" * 40) + "\n" + grid
    .map(_
    .map(_.toString).mkString(" ")
    ).mkString("\n")


  def fromStringToGrid(s: String): TGrid = {
    val rows: Array[String] = s.split(",")
    def f(i: Int, j: Int, c: Char): GridElement = c match {
      case '0' => EmptyGrid(Position(i, j))
      case '1' => WareGrid(Position(i, j), None)
      case '2' => PackGrid(Position(i, j))
    }
    rows.zipWithIndex.map {
      case (row, i) => row.zipWithIndex
        .map {
        case (character, j) => f(i, j, character)
      }.toArray
    }
  }

  def fromGridToString(): String = {
    def f(g: GridElement): Char = g match {
      case e: EmptyGrid => '0'
      case w: WareGrid => '1'
      case p: PackGrid => '2'
      case o: OccupiedGrid => '3'
    }
    grid.map {
      case gridArray =>
        gridArray.map {
          case gridElem => f(gridElem)
        }.mkString("")
    }.mkString(",")
  }
}

object Grid extends App {
  type TGrid = Array[Array[GridElement]]
  val positions = "1110011,0011001,0011001,1011001,1011001,0000000,0022200"
  val numRobots = 6
  val orderMaxSize = 50
  val dlTime = 5
  println(Grid(positions))
  println(Grid(positions).fromGridToString())
}