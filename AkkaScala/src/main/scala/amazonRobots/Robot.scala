package amazonRobots

import amazonRobots.Protocol._
import scala.concurrent.{Await, ExecutionContext}
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.actor._
import akka.util.Timeout
import amazonRobots.Protocol.Position
import amazonRobots.Protocol.NextPosition
import scala.util.Random

/**
 * Created by sacry on 17/06/14.
 */
class Robot(val initPos: Position, val grid: Grid, system: ActorSystem) extends Actor {
  implicit val timeout = Timeout(1 seconds)

  override def toString = RobotsRepository.actorNameByRef(self).toString

  private var lastPos: Position = initPos
  private var currPos: Position = initPos
  private var trail: List[Position] = List.empty

  def position: Position = Position(currPos.x, currPos.y)

  private var neighbourRobots: List[Position] = List.empty
  private var nextMoveByOthers: List[Position] = List.empty
  private var nextMove = Position(-1, -1)
  private var ACK_received = false

  def freePosition: Position = {
    val access = grid.accessibleNeighbors(position)
    position :: trail
    val realAccess = for (elem <- access if !nextMoveByOthers.contains(elem)) yield elem
    if (realAccess.isEmpty)
      position
    else
      realAccess(Random.nextInt(realAccess.size))
  }

  def receive: Receive = {
    case Move => {
      reset
      neighbourRobots = grid.occupiedNeighbors(position)
      nextMove = freePosition
      if (neighbourRobots.isEmpty)
        system.actorSelection("/user/*") ! NextPosition(nextMove.x, nextMove.y)
      else
        neighbourRobots map (pos => system.actorSelection("/user/*") ! AskPosition(pos.x, pos.y))
    }
    case p: AskPosition => {
      sender() ! position
    }
    case p: Position => {
      sender() ! NextPosition(nextMove.x, nextMove.y)
    }
    case p: NextPosition => {
      if (nextMove != p) {
        p :: nextMoveByOthers
        sender() ! ACK
        debugIt("ACK send: " + this.toString)
      } else {
        sender() ! NACK
        debugIt("NACK send: " + this.toString)
        self ! ACK
      }
    }
    case ACK => {
      if(!ACK_received){
        debugIt("ACK received: " + this.toString)
        context.actorSelection("/user/*") ! GUIPosition(position, nextMove)
        lastPos = currPos
        currPos = nextMove
        ACK_received = true
      }
    }
    case NACK => {
      debugIt("NACK received: " + this.toString)
      nextMove :: nextMoveByOthers
      nextMove = freePosition
      neighbourRobots map (pos => system.actorSelection("/user/*") ! AskPosition(pos.x, pos.y))
    }
    case Shutdown => self ! PoisonPill
  }

  def reset {
    neighbourRobots = List.empty
    nextMoveByOthers = trail
    nextMove = Position(-1, -1)
    ACK_received = false
  }

  def debugIt(msg: String) {
    if (false)
      println(msg)
  }
}


object Robot {

}
