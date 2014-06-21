package amazonRobots

import akka.actor.{ActorRef, Props, ActorSystem}
import amazonRobots.Protocol.{Update}
import scala.concurrent.ExecutionContext.Implicits.global
import RobotsRepository._

/**
 * Created by Swaneet on 17.06.2014.
 */
object Simulation {
  def apply(gridString: String, numRobots: Int = 6, orderMaxSize: Int = 50, verbose: Boolean = false) =
    new Simulation(gridString, numRobots, orderMaxSize, verbose)
}


class Simulation(gridString: String, numRobots: Int, orderMaxSize: Int, verbose: Boolean) {

  val system = ActorSystem("NaSC")
  val realWorld = Grid(gridString)
  val staticGrid = Grid(gridString)

  if (verbose) println(s"Initial grid: $realWorld")

  val robots: List[ActorRef] =
    generateRobotNames(numRobots).map(c =>
      system.actorOf(Props(classOf[Robot], realWorld.newRobPosition(c), staticGrid))
    ).toList

  val renderer = system.actorOf(Props(classOf[Renderer], realWorld, robots))

  val articles = RobotsRepository.articles(realWorld)
  val orders = RobotsRepository.orders(realWorld)

  def run(ms: Long) = {

  }

  import scala.concurrent.duration._

  system.scheduler.schedule(0 milliseconds, 1 seconds, renderer, Update)

  // robots(0) ! "find others"

}


