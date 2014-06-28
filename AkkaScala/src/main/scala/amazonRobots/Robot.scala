package amazonRobots

import scala.concurrent.{ExecutionContext}
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

  def position: Position = Position(currPos.x, currPos.y)

  def generateFreePosition: Position = {
    val access = grid.accessibleNeighbors(position).filterNot(p => reservedPositions.contains(p))
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

  private var currPos: Position = initPos
  private var robotsPriority: List[(ActorRef, Priority)] = List.empty

  def receive: Receive = {
    case Move => {
      reset
      system.actorSelection("/user/*") ! Priority(priority)
    }
    case p: Priority => {
      robotsPriority = (sender(), p) :: robotsPriority
      if (robotsPriority.size >= numRobots) {
        robotsPriority = robotsPriority.sortBy(_._2.priority)
        if (robotsPriority.head._1 == self) {
          debugIt(robotsPriority.map(t => RobotsRepository.actorNameByRef(t._1) + " " + t._2).mkString("\n"))
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
      reservedPositions = Position(x, y) :: reservedPositions
    }
    case e => {
      debugIt(this.toString + " case e => " + e.toString)
    }
  }

  private var k: Int = 0
  private var acks: Int = 0
  private var nextMove: Position = Position(-1, -1)
  private var reservedPositions: List[Position] = List.empty

  def moving: Receive = {
    case Move => {
      nextMove = generateFreePosition
      val lower = lowerRobots
      k = lower.size
      acks = 0
      if (k == 0)
        self ! ACK
      else {
        lower map {
          case (actor, prio) => actor ! AskPosition(nextMove.x, nextMove.y)
        }
      }
    }
    case ACK => {
      acks += 1
      if (acks >= k) {
        debugIt(currPos + ", " + nextMove + ", " + this.toString)
        system.actorSelection("/user/*") ! ReservePosition(nextMove.x, nextMove.y)
        lowerRobots.headOption match {
          case Some((actor, _)) => actor ! Ticket
          case None => "fin"
        }
        renderer ! GUIPosition(position, nextMove)
        currPos = nextMove
        context.become(receive)
      }
    }
    case NACK => {
      println("gibts nicht")
    }
    case _: ReservePosition => println("ich kenne meine pos")
  }

  def lowerRobots = {
    robotsPriority.dropWhile {
      case (actor, _) => actor != self
    }.drop(1)
  }

  def reset {
    robotsPriority = List.empty
    reservedPositions = List.empty
    k = 0
    acks = 0
    nextMove = Position(-1, -1)
  }

  def debugIt(msg: String) {
    if (true)
      println(msg)
  }
}


object Robot {

}
