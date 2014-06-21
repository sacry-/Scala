package amazonRobots

import amazonRobots.Protocol.Position
import RobotsRepository._

import scala.util.Random

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

case class OccupiedBlock(p: Position, c: Char) extends Block {
  override def toString = c.toString
}

case class Grid(val positions: String) extends AbstractGrid with BlockOperations with GridConverter {
  self =>

  val grid: TGrid = fromStringToGrid(positions)

  override def toString = ("-" * 40) + "\n" + grid
    .map(_.map(_.toString).mkString(" ")
    ).mkString("\n")

  def accessiblePositions: List[Position] = {
    val openGrid: Array[Block] = grid.flatMap(_.filter(gridElem => isAccessible(gridElem.p)))
    openGrid.map(gridElem => gridElem.p).toList
  }

  def neighbors(p: Position): List[Position] = {
    def inBounds(t: Int) = 0 <= t && t < grid.size
    List((p.x + 1, p.y), (p.x - 1, p.y), (p.x, p.y + 1), (p.x, p.y - 1))
      .filter(t => inBounds(t._1) && inBounds(t._2))
      .map(t => Position(t._1, t._2))
  }

  def traversableNeighbors(p: Position): List[Position] = {
    self.neighbors(p).filter(isTraversable(_))
  }

  def accessibleNeighbors(p: Position): List[Position] = {
    self.neighbors(p).filter(isAccessible(_))
  }

  def newRobPosition(c: Char): Position = {
    val pos = Random.shuffle(self.accessiblePositions).head
    self.occupyPosition(pos, c)
    pos
  }
}


trait AbstractGrid {
  type TGrid = Array[Array[Block]]
  val grid: TGrid
}

trait BlockOperations extends AbstractGrid {

  def blockAt(p: Position): Block = grid(p.x)(p.y)

  def isAccessible(p: Position): Boolean = {
    blockAt(p).isInstanceOf[Accessible]
  }

  def isOccupied(p: Position): Boolean = {
    blockAt(p).isInstanceOf[OccupiedBlock]
  }

  def isBlocked(p: Position): Boolean = {
    !blockAt(p).isInstanceOf[Accessible]
  }

  def isTraversable(p: Position) = {
    isAccessible(p) || isOccupied(p)
  }

  def occupyPosition(p: Position, c: Char) {
    grid(p.x).update(p.y, OccupiedBlock(p, c))
  }

  def leavePosition(p: Position) {
    grid(p.x).update(p.y, EmptyBlock(p))
  }

  def move(source: Position, target: Position, c: Char) {
    println(s"move($source, $target, $c")
    val accessible = isAccessible(target)
    if (accessible) {
      occupyPosition(target, c)
      leavePosition(source)
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
      case c: Char => OccupiedBlock(Position(i, j), c)
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
      case o: OccupiedBlock => o.c
    }
    grid.map {
      case gridElemArray =>
        gridElemArray.map {
          case gridElem => fromGridElemToChar(gridElem)
        }.mkString("")
    }.mkString(",")
  }

}