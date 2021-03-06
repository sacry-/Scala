package amazonRobots

import akka.actor.ActorRef

/**
 * Created by sacry on 16/06/14.
 */
object Protocol {

  sealed trait Message

  case class AskPosition(x: Int, y: Int) extends Message

  case class Position(x: Int, y: Int) extends Message

  case class ReservePosition(x: Int, y: Int) extends Message

  case class Priority(priority: Double) extends Message

  case object Ticket extends Message

  lazy val nilPos = Position(-1,-1)
  val isNilPos: Position => Boolean = _ == nilPos

  // depre
  case object ACK extends Message

  case object NACK extends Message

  case object Shutdown extends Message

  // GUI

  sealed trait GUI

  case object Update extends GUI

  case object Move extends GUI

  case class GUIPosition(source: Position, target: Position) extends GUI

}
