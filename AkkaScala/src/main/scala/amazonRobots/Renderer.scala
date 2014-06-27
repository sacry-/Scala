package amazonRobots

import akka.actor.{ActorSystem, ActorSelection, ActorRef, Actor}
import Protocol._

/**
 * Created by Swaneet on 19.06.2014.
 */
class Renderer(system: ActorSystem, g: Grid) extends Actor {

  import scala.collection.mutable.Queue

  @volatile private var positions: Queue[(ActorRef, Position, Position)] =
    Queue.empty[(ActorRef, Position, Position)]

  def receive = {
    case Update => {
      while (positions.nonEmpty) {
        val (actor, source, target) = positions.dequeue()
        val name = RobotsRepository.actorNameByRef(actor)
        g.move(source, target, name)
      }
      println(getFalseStates)
      println(g)
      system.actorSelection("/user/*") ! Move
    }
    case GUIPosition(source: Position, target: Position) => {
      //println("position from: " + RobotsRepository.actorNameByRef(sender()))
      positions.enqueue((sender(), source, target))
    }
  }

  def getFalseStates: String = {
    var msg = ""
    val robots = 'A' until 'G' toList

    val inter = g.fromGridToString().filter(c =>
      c >= robots.head && c <= robots.last
    ).toList

    if (inter.toSet.size != inter.size)
      msg += "Error: Duplicated Robots\n"

    val robotsNotInView = robots.foldLeft(List[Char]()) {
      case (acc, c) if !inter.contains(c) => c :: acc
      case (acc, _) => acc
    }

    if (robotsNotInView.nonEmpty)
      msg += s"Error: Robot Vanished {${robotsNotInView.mkString(", ")}}\n"

    msg
  }

}
