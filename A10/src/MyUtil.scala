
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Try

/**
 * Created by sacry on 14/06/14.
 */
object MyUtil {

  def await[A](x: Future[A]) = {
    Await.ready(x, Duration.Inf)
    println(x.value)
    x
  }

  def exec[T](name: String, delay: Long, block: => T): T = {
    println(s"Started ${name}")
    Thread sleep delay
    val r = block
    r
  }
}
