package amazonRobots

import amazonRobots.Protocol._
import scala.concurrent.{Await, ExecutionContext}
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.actor._
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import akka.routing.Broadcast

/**
 * Created by sacry on 17/06/14.
 */
class Robot(val initPos: Position, val grid: Grid) extends Actor {
  implicit val timeout = Timeout(5 seconds)
  val name = self.path.name

  private var currPos: Position = initPos

  def position: Position = Position(currPos.x, currPos.y)

  def shortestPath(p1: Position) = ???

  def receive: Receive = {
    case AskPositions => {
      val others = context.actorSelection("/user/*") ask Position
      others.map {
        case p: Position => (p.x, p.y)
      }
    }
    case Position => sender() ! position
    case nextPosition: NextPosition => sender() ! nextPosition
    case Move => currPos = RobotsRepository.randomPosition(grid, position)
    case Shutdown => self ! PoisonPill
  }
}


object Robot {

}
