package amazonRobots

import amazonRobots.Protocol.Position
import amazonRobots.Grid.dlTime

/**
 * Created by sacry on 17/06/14.
 */
case class Order(articles: List[Article]) {
  val size = articles.map(_.productSize).sum
  val numProducts = articles.size
  val unloadTime = numProducts * dlTime
}

case class Article(productSize: Int, name: String, productPos: Position) {
  def whereCanIGetYou = ???
}