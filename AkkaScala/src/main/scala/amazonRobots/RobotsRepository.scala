package amazonRobots

import amazonRobots.Protocol.Position
import akka.actor.{ActorRef, Props, ActorSystem}
import scala.util.Random

/**
 * Created by Swaneet on 19.06.2014.
 */
object RobotsRepository {

  val dlTime = 5000L

  def articles(g: Grid): List[Article] = {
    List(
      Article(5, "Cherry", Position(3, 0)),
      Article(8, "Chocolate", Position(0, 1)),
      Article(150, "Mac", Position(4, 2))
    )
  }

  def randomPosition(g: Grid, p: Position): Position = {
    val traversables = g.traversableNeighbors(p)
    traversables(Random.nextInt(traversables.size))
  }

  def orders(g: Grid) = {
    List(
      Order(articles(g)),
      Order(List(articles(g)(2), articles(g)(1))),
      Order(List(articles(g)(0)))
    )
  }

  def generateRobotNames(numOfRobots: Int) = {
    (65 until (65 + numOfRobots)).map(_.toChar)
  }

  case class Article(productSize: Int, name: String, productPos: Position)

  case class Order(articles: List[Article]) {
    val size = articles.map(_.productSize).sum
    val numProducts = articles.size
    val unloadTime = numProducts * dlTime
  }

}
