package amazonRobots

import akka.actor.{ActorRef, Actor}
import Protocol._

/**
 * Created by Swaneet on 19.06.2014.
 */
class Renderer(g:Grid, ls:List[ActorRef]) extends Actor {
  import scala.collection.mutable.Map
  val positions:Map[ActorRef,Position] = Map.empty
  def receive = {
    case Update => {
      ls foreach( ref => ref ! Ask )
      ls(1) ! Move
      println(g)
    }
    case p@Position(x,y) => {
      val actor = sender()
      println(actor.path.name + " is at " + p)
      val maybeOldPos = positions.get(actor)
      lazy val oldPos = maybeOldPos.get
      maybeOldPos match {
        case None => positions.put(actor, p)
        case Some(`p`) => ()
        case Some(oldPos) => {
          g.move(oldPos,p, actor.path.name.charAt(1)) // böse
          positions.put(actor, p)
        }
      }
    }
  }

}
