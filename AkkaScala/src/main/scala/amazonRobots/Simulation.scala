package amazonRobots

import akka.actor.{Props, ActorSystem}
import amazonRobots.Protocol.Position

/**
 * Created by Swaneet on 17.06.2014.
 */
class Simulation(
                 val gridString: String,
                 val numRobots: Int = 6,
                 val orderMaxSize: Int = 50,
                 val dlTime: Long = 5000L,
                 val verbose: Boolean = false)  // schade, dass man dieses "verbose" nicht implicit machen kann...
{
  import Simulation.{Article,Order}


  // initialization
  val system = ActorSystem("NitscheAndSahooCroporation")
  val grid = new Grid(gridString)

  if (verbose) println(s"Initial grid: $grid")

  // insert robots
  val robots = for (i <- 1 to numRobots)
    yield system.actorOf(Props(classOf[Robot], ("Robot #" + i), grid.newRobPosition))

  if (verbose) robots.foreach(println)
  if (verbose) println(s"Occupied Grid: $grid")

  // es können die Artikel und Orders erzeugt werden.
  val sampleArticle = new Article(5, "Cherry", Position(0, 0),grid)
  if (verbose) println(sampleArticle)
  val sampleOrder = new Order(List(sampleArticle,sampleArticle,sampleArticle),dlTime)
  if (verbose) println(sampleOrder)

  def run(ms: Long) = {
    // here Code that runs "ms" milliseconds of the virtual world
    // changing positions of robots, if the 5s of changing is elapsed
    // decrementing remaining time of (un)loading articles.
  }
}

object Simulation {
  case class Order(articles: List[Article], dlTime:Long) {
    val size = articles.map(_.productSize).sum
    val numProducts = articles.size
    val unloadTime = numProducts * dlTime
    override def toString = "Order("+articles.toString+")"
  }

  case class Article(productSize: Int, name: String, productPos: Position,private val grid:Grid) {
    def whereCanIGetYou: List[Position] = grid.accessibleNeighbors(productPos)
    override def toString = s"Article($productSize,$name,$productPos)"  // ignore grid in tostring
  }
}

