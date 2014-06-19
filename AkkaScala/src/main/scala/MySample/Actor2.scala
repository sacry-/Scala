package MySample

import akka.actor._
import akka.pattern.{ask, pipe}
import scala.concurrent.{ExecutionContext}
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.routing.RoundRobinRouter
import scala.util.Random

/**
 * Created by sacry on 18/06/14.
 */
object ActorFactory {

  case class Order(name: String, amount: Int) {
    override def toString = "Order(" + name + "," + amount + ")"
  }

  def randomOrder: Order = {
    val name = Random.alphanumeric.take(6).mkString
    val amount = Random.nextInt(10)
    Order(name, amount)
  }

  def randomDuration: Long = {
    (500 + Random.nextInt(2000)) toLong
  }
}

object Actor2 extends App {

  import ActorFactory._

  val system = ActorSystem("MySystem")

  val newOrder = Request(Order("new", 0), 0L)
  system.actorOf(Manager.props(5), "Manager1") ! newOrder

  sealed trait Message

  case class Request(order: Order, time: Long) extends Message

  case class Response(order: Order, time: Long) extends Message

  case object Shutdown extends Message

  object Manager {
    def props(n: Int): Props = Props(new Manager(n))
  }

  class Manager(n: Int) extends Actor {
    val workerRouter = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(n)), name = "myRouter")
    var hours = 0

    override def receive: Receive = {
      case Request(Order("new", 0), 0L) => {
        if (hours < 10) {
          val (order, t) = (randomOrder, randomDuration)
          println(s"Requesting ${order}. Will take around ${t} MilliSeconds.")
          hours += 1
          workerRouter ! Request(order, t)
        } else {
          workerRouter ! "tired"
        }
      }
      case Shutdown => {
        println("Workers are exhausted!");
        context.stop(workerRouter)
        println("Manager still works.. :(")
      }
    }
  }

  class Worker() extends Actor {
    override def receive: Receive = {
      case "tired" => {
        println("Some Sleep at last! :)")
        sender() ! Shutdown
      }
      case Request(order, t) => {
        println(s"${self.path} starts working on ${order} for ${t} seconds")
        Thread.sleep(t)
        sender() ! Request(Order("new", 0), 0L)
      }
    }
  }

}