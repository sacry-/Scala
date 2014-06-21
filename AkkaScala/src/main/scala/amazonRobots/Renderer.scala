package amazonRobots

import akka.actor.{ActorRef, Actor}
import Protocol._

/**
 * Created by Swaneet on 19.06.2014.
 */
class Renderer(g: Grid, ls: List[ActorRef]) extends Actor {

  import scala.collection.mutable.Map

  val positions: Map[ActorRef, Position] = Map.empty
  val DUMMY = 'X'

  def actorNameByRef(actor: ActorRef): Char = {
    val nameAsList = actor.path.name.toList
    if (nameAsList.size > 0)
      return nameAsList.last.toUpper
    DUMMY
  }

  def receive = {
    case Update => {
      ls foreach (ref => ref ! Ask)
      ls foreach (ref => ref ! Move)
      println(g)
    }
    case p@Position(x, y) => {
      val actor = sender()
      val maybeOldPos = positions.get(actor)
      maybeOldPos match {
        case None => positions.put(actor, p)
        case Some(`p`) => ()
        case Some(oldPos) => {
          g.move(oldPos, p, actorNameByRef(actor))
          positions.put(actor, p)
        }
      }
    }
  }

}
