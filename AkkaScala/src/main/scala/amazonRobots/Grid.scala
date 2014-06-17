package amazonRobots

import amazonRobots.Protocol.Position

/**
 * Created by sacry on 17/06/14.
 */
trait Block {
  def p: Position

  def x: Int = p.x

  def y: Int = p.y
}

sealed trait Accessible

case class EmptyBlock(p: Position) extends Block with Accessible {
  override def toString = "0"
}

case class WareBlock(p: Position, article: Option[Article]) extends Block {
  override def toString = "1"
}

case class PackBlock(p: Position) extends Block {
  override def toString = "2"
}

case class OccupiedBlock(p: Position) extends Block {
  override def toString = "3"
}

case class Grid(positions: String) extends GridOperations with GridConverter {

  val grid: TGrid = fromStringToGrid(positions)

  override def toString = ("-" * 40) + "\n" + grid
    .map(_.map(_.toString).mkString(" ")
    ).mkString("\n")

  def allOpenPositions(): List[Position] = {
    val openGrid: Array[Block] = grid.flatMap(_.filter(gridElem => isAccessible(gridElem.p)))
    openGrid.map(gridElem => gridElem.p).toList
  }

}

object Grid extends App {
  val orderMaxSize = 50
  val dlTime = 5
}

trait AbstractGrid {
  type TGrid = Array[Array[Block]]

  val grid: TGrid
}

trait GridOperations extends AbstractGrid {

  def isAccessible(p: Position): Boolean = {
    grid(p.x)(p.y).isInstanceOf[Accessible]
  }

  def isOccupied(p: Position): Boolean = {
    grid(p.x)(p.y).isInstanceOf[OccupiedBlock]
  }

  def occupyPosition(p: Position) {
    if (isAccessible(p)) {
      grid(p.x).update(p.y, OccupiedBlock(p))
    }
  }

  def leavePosition(p: Position) {
    if (isOccupied(p)) {
      grid(p.x).update(p.y, EmptyBlock(p))
    }
  }
}

trait GridConverter extends AbstractGrid {
  def fromStringToGrid(s: String): TGrid = {
    val rows: Array[String] = s.split(",")
    def fromCharToGridElem(i: Int, j: Int, c: Char): Block = c match {
      case '0' => EmptyBlock(Position(i, j))
      case '1' => WareBlock(Position(i, j), None)
      case '2' => PackBlock(Position(i, j))
      case '3' => OccupiedBlock(Position(i, j))
    }
    rows.zipWithIndex.map {
      case (row, i) => row.zipWithIndex
        .map {
        case (character, j) => fromCharToGridElem(i, j, character)
      }.toArray
    }
  }

  def fromGridToString(): String = {
    def fromGridElemToChar(g: Block): Char = g match {
      case e: EmptyBlock => '0'
      case w: WareBlock => '1'
      case p: PackBlock => '2'
      case o: OccupiedBlock => '3'
    }
    grid.map {
      case gridElemArray =>
        gridElemArray.map {
          case gridElem => fromGridElemToChar(gridElem)
        }.mkString("")
    }.mkString(",")
  }

}