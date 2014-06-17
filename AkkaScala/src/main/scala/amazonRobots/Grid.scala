package amazonRobots

import amazonRobots.Protocol.Position
import akka.actor.ActorRef

/**
 * Created by sacry on 17/06/14.
 */
trait GridElement {
  def p: Position

  def x: Int = p.x

  def y: Int = p.y

  def x_y: (Int, Int) = (x, y)
}

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

case class OccupiedGrid(p: Position) extends GridElement {
  override def toString = "3"
}

trait AbstractGrid {
  type TGrid = Array[Array[GridElement]]

  val grid: TGrid
}

trait GridOperations extends AbstractGrid {

  def isAccessible(p: Position): Boolean = {
    grid(p.x)(p.y).isInstanceOf[Accessible]
  }

  def isOccupied(p: Position): Boolean = {
    grid(p.x)(p.y).isInstanceOf[OccupiedGrid]
  }

  def occupyPosition(p: Position) {
    if (isAccessible(p)) {
      grid(p.x).update(p.y, OccupiedGrid(p))
    }
  }

  def leavePosition(p: Position) {
    if (isOccupied(p)) {
      grid(p.x).update(p.y, EmptyGrid(p))
    }
  }
}

case class Grid(positions: String) extends GridOperations with GridConverter {

  val grid: TGrid = fromStringToGrid(positions)

  override def toString = ("-" * 40) + "\n" + grid
    .map(_.map(_.toString).mkString(" ")
    ).mkString("\n")

  def allOpenPositions(): List[Position] = {
    val openGrid: Array[GridElement] = grid.flatMap(_.filter(_.isInstanceOf[EmptyGrid]))
    openGrid.map(g => g.p).toList
  }

}

object Grid extends App {
  val orderMaxSize = 50
  val dlTime = 5
}


trait GridConverter extends AbstractGrid {

  def fromStringToGrid(s: String): TGrid = {
    val rows: Array[String] = s.split(",")
    def fromCharToGridElem(i: Int, j: Int, c: Char): GridElement = c match {
      case '0' => EmptyGrid(Position(i, j))
      case '1' => WareGrid(Position(i, j), None)
      case '2' => PackGrid(Position(i, j))
      case '3' => OccupiedGrid(Position(i, j))
    }
    rows.zipWithIndex.map {
      case (row, i) => row.zipWithIndex
        .map {
        case (character, j) => fromCharToGridElem(i, j, character)
      }.toArray
    }
  }

  def fromGridToString(): String = {
    def fromGridElemToChar(g: GridElement): Char = g match {
      case e: EmptyGrid => '0'
      case w: WareGrid => '1'
      case p: PackGrid => '2'
      case o: OccupiedGrid => '3'
    }
    grid.map {
      case gridElemArray =>
        gridElemArray.map {
          case gridElem => fromGridElemToChar(gridElem)
        }.mkString("")
    }.mkString(",")
  }

}