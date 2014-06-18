package MySample

import akka.actor._
import akka.pattern.{ask, pipe}
import scala.concurrent.{ExecutionContext}
import ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
 * Created by sacry on 18/06/14.
 */
object Manager {
  def props(magicNumber: Long): Props = Props(new Manager(magicNumber))
}

class Manager(x: Long) extends Actor {
  override def receive: Receive = {
    case 10 => println("Fuck you!")
    case _ => println("Manager" + x)
  }
}

object Worker {
  def props(x: Int): Props = Props(new Worker(x))
}

class Worker(x: Int) extends Actor {
  override def receive: Receive = {
    case 10 => {
      val fu = self.ask("Hello!")(5 seconds)
      fu pipeTo sender()
    }
    case i => println("Worker" + x + " " + i)
  }
}

object Main extends App {
  val system = ActorSystem("MySystem")

  val mg1 = system.actorOf(Manager.props(100), "Manager1")
  val mg2 = system.actorOf(Manager.props(101), "Manager2")

  val w1 = system.actorOf(Worker.props(1), "Worker1")
  val w2 = system.actorOf(Worker.props(2), "Worker2")
  //w2 ! "Hello!"
 w2.ask(10, mg1)(5 seconds)

}