package MySample

import scala.concurrent.{ExecutionContext, Await, Future}
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.actor._
import akka.event.Logging
import akka.pattern.{ask, pipe}
import akka.util._
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import akka.actor.FSM.Failure
import akka.util.Timeout

/**
 * Created by sacry on 17/06/14.
 */
object DemoActor extends App {
  val system = ActorSystem("mySystem")
  val actor1 = system.actorOf(Props(classOf[DemoActor], 6, 0))
  val actor2 = system.actorOf(Props(classOf[DemoActor], 3, 1))
  val actor3 = system.actorOf(Props(classOf[DemoActor], 23, 2))
  val actor4 = system.actorOf(Props(classOf[DemoActor], 34, 3))

  val actors = List(actor1,actor2,actor3,actor4)

  //actor1 ! 0
  actor1.tell(0, system.deadLetters)

  Thread.sleep(600)
  system.shutdown()

  def nextActor(n:Int):ActorRef = {
    actors( (n + 1) % actors.size )
  }
}

class DemoActor(magicNumber: Int, id:Int) extends Actor {
  implicit val timeout = Timeout(5 seconds)
  import DemoActor._
  def receive = {
    case x:Int if x > 4000 => {
      val s = "Finished with " + x
      println(s)
      sender() ! s
    }
    case x: Int => {
      println(id + ": " + x)
      Thread.sleep(10);
      val resp = nextActor(id) ask (x + magicNumber)
      resp map { x => { val s = id + x.toString; println(s); s } } pipeTo ( sender() )
    }
    case _ => {
      println(id +": unknown")
    }
  }
}