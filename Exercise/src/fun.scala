import scala.concurrent.duration.Duration
import scala.util.{Random, Failure, Success, Try}
import scala.concurrent._
import scala.concurrent.Future
import scala.util.Try
import ExecutionContext.Implicits.global

/**
 * Created by sacry on 21/06/14.
 */
object Fun extends App {

  trait Calculate {
    def votes: Int

    def percentage(total: Int): Int = {
      Try((votes.toDouble / total) * 100) match {
        case Success(x) => x.toInt
        case Failure(_) => 0
      }
    }
  }

  case class Nation(name: String) extends Calculate {
    @volatile private var internalVotes = 0

    def votes = internalVotes

    def updateVote(n: Int = 1): Future[Int] = {
      Future {
        Thread.sleep(Random.nextInt(1000))
        internalVotes += n
        internalVotes
      }
    }

    override def toString = s"${name} (${internalVotes} votes)"
  }

  def showVotes(nations: (Nation, Nation)): String = {
    val (n1, n2) = nations
    val total = n1.votes + n2.votes
    val t = List(n1, n2) map (country => country.percentage(total))
    s"${n1} ${t.map(x => s"${x}%").mkString(" - ")} ${n2} || total: ${total}"
  }

  val (ghana, bh) = (Nation("Ghana"), Nation("BH"))

  println(showVotes((ghana, bh)))
  (0 until 10) foreach {
    idx =>
      val xs = List(Random.nextInt(20), Random.nextInt(15))
      Await.ready(ghana.updateVote(xs.head), Duration.Inf)
      Await.ready(bh.updateVote(xs.last), Duration.Inf)
      val r = s"${idx + 1}. ${showVotes((ghana, bh))} (${xs.head} , ${xs.last}) diff=${xs.max - xs.min}"
      println(r)
  }
}
