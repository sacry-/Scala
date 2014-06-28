package amazonRobots

import scala.concurrent.duration._
import akka.actor._
import akka.util.Timeout
import amazonRobots.Protocol._

import scala.util.Random

/**
 * Created by sacry on 17/06/14.
 */
class Robot(val initPos: Position, val grid: Grid, system: ActorSystem, renderer: ActorRef, numRobots: Int) extends Actor {
  implicit val timeout = Timeout(1 seconds)

  override def toString = RobotsRepository.actorNameByRef(self).toString

  private var currPos: Position = initPos

  def position: Position = Position(currPos.x, currPos.y)

  private var state = new State(self)

  def generateFreePosition: Position = {
    val access = grid.accessibleNeighbors(position).filterNot(state.isAlreadyReserved(_))
    if (access.isEmpty)
      position
    else
      access(Random.nextInt(access.size))
  }

  def shortestPath: List[Position] = {
    val all = grid.allAccessiblePositions
    val randomPos = all(Random.nextInt(all.size))
    // Dijkstra
    List.empty
  }

  def priority: Double = Random.nextDouble()

  def receive: Receive = {
    case Move => {
      state = new State(self)
      system.actorSelection("/user/*") ! Priority(priority)
    }
    case p: Priority => {
      state.addRobotsPriority(sender(), p)
      if (state.robotsPriority.size >= numRobots) {
        state.robotsSort
        if (state.isFirstPrioritized) {
          debugIt(state.robotsPriority.map(t => RobotsRepository.actorNameByRef(t._1) + " " + t._2).mkString("\n"))
          self ! Ticket
        }
      }
    }
    case Ticket => {
      context.become(moving)
      self ! Move
    }
    case AskPosition(x, y) => {
      // todo
      sender() ! ACK
    }
    case ReservePosition(x, y) => {
      state.reservePosition(x, y)
    }
    case e => {
      debugIt(this.toString + " case e => " + e.toString)
    }
  }

  def moving: Receive = {
    case Move => {
      state.nextMove = generateFreePosition
      val lower = state.lowerRobots
      state.acks = 0
      state.k = lower.size
      if (state.k == 0)
        self ! ACK
      else {
        val (x, y) = state.nextMoveAsTuple
        lower map {
          case (actor, prio) => actor ! AskPosition(x, y)
        }
      }
    }
    case ACK => {
      state.acks += 1
      if (state.acks >= state.k) {
        debugIt(currPos + ", " + state.nextMove + ", " + this.toString)
        val (x, y) = state.nextMoveAsTuple
        system.actorSelection("/user/*") ! ReservePosition(x, y)
        state.lowerRobots.headOption match {
          case Some((actor, _)) => actor ! Ticket
          case None => "fin"
        }
        renderer ! GUIPosition(position, state.nextMove)
        currPos = state.nextMove
        context.become(receive)
      }
    }
    case NACK => {
      println("gibts nicht")
    }
    case _: ReservePosition => println("ich kenne meine pos")
  }

  def debugIt(msg: String) {
    if (true)
      println(msg)
  }
}


class State(me: ActorRef) {

  var k: Int = 0
  var acks: Int = 0
  var nextMove: Position = Position(-1, -1)
  var reservedPositions: List[Position] = List.empty
  var robotsPriority: List[(ActorRef, Priority)] = List.empty

  def nextMoveAsTuple: (Int, Int) = {
    (nextMove.x, nextMove.y)
  }

  def isAlreadyReserved(p: Position): Boolean = {
    reservedPositions.contains(p)
  }

  def reservePosition(x: Int, y: Int) {
    reservedPositions = Position(x, y) :: reservedPositions
  }

  def addRobotsPriority(sender: ActorRef, priority: Priority) {
    robotsPriority = (sender, priority) :: robotsPriority
  }

  def robotsSort {
    robotsPriority = robotsPriority.sortBy(_._2.priority)
  }

  def isFirstPrioritized: Boolean = {
    robotsPriority.head._1 == me
  }

  def lowerRobots = {
    robotsPriority.dropWhile {
      case (actor, _) => actor != me
    }.drop(1)
  }

}