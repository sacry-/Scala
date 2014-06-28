package amazonRobots

import akka.actor.{ActorRef, Props, ActorSystem}
import amazonRobots.Protocol.{Update}
import scala.concurrent.ExecutionContext.Implicits.global
import RobotsRepository._

/**
 * Created by Swaneet on 17.06.2014.
 */
object Simulation {
  def apply(gridString: String,
            numRobots: Int = 6,
            orderMaxSize: Int = 50) =
    new Simulation(gridString, numRobots, orderMaxSize)
}


class Simulation(gridString: String, numRobots: Int, orderMaxSize: Int) {

  val system = ActorSystem("NaSC")
  val realWorld = Grid(gridString)
  val staticGrid = Grid(gridString)

  val renderer = system.actorOf(Props(classOf[Renderer], system, realWorld))
  val robots: List[ActorRef] =
    generateRobotNames(numRobots).map(c =>
      system.actorOf(Props(classOf[Robot], realWorld.newRobPosition(c), realWorld, system, renderer, numRobots), name = c.toString)
    ).toList

  val articles = RobotsRepository.articles(realWorld)
  val orders = RobotsRepository.orders(realWorld)

  import scala.concurrent.duration._

  system.scheduler.schedule(1 seconds, 5 seconds, renderer, Update)

}


