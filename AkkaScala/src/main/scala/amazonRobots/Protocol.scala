package amazonRobots

import akka.actor.ActorRef

/**
 * Created by sacry on 16/06/14.
 */
object Protocol {
  
  sealed trait Message

  case object AskPositions extends Message

  case class Position(x: Int, y: Int) extends Message

  case class NextPosition(x: Int, y: Int) extends Message

  case object Shutdown extends Message

  // GUI

  sealed trait GUI

  case object Update extends GUI

  case object Move extends GUI

}
