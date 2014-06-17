package amazonRobots

import akka.actor.{Props, ActorSystem}
import scala.util.Random
import amazonRobots.Protocol.Position

/**
 * Created by sacry on 16/06/14.
 */
object Main {

  val system = ActorSystem()
  val numRobots = 6
  val positions = "1110011,0011001,0011001,1011001,1011001,0000000,0022200"
  val grid = Grid(positions)

  def main(args: Array[String]): Unit = run()

  def run(): Unit = {
    println(grid)

    def eraseOpen: Position = {
      Random.shuffle(grid.allOpenPositions).head
    }

    val robots = for (i <- 1 to numRobots) yield system.actorOf(Props(classOf[Robot], ("Robot" + i), eraseOpen))
    robots.foreach(println)
  }
}