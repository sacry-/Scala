import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.util._
import ExecutionContext.Implicits.global

/**
 * Created by sacry on 27/05/14.
 */
object Concurrency extends App {

  future {
    1 + 2
  } onSuccess {
    case e => println(e)
  }

  future {
    1 / 0
  } onFailure {
    case e: Throwable => println(e)
  }

  future {
    0.0 / 0.0
  } onComplete {
    case Success(e) => println(e)
    case Failure(e) => println(e)
  }

  val f4 = future {
    val t = System.currentTimeMillis()
    while ((System.currentTimeMillis() - t) < 2000L) {}
    2 + 4
  }
  println("Started!")
  Await.result(f4, Duration(3000, "milli"))
  println("Got: " + f4.value.get.getOrElse(0))

  lazy val f5 = Future {
    val t = System.currentTimeMillis()
    while ((System.currentTimeMillis() - t) < 2000L) {}
    2 + 4
  }
  lazy val f6 = Future {
    val t = System.currentTimeMillis()
    while ((System.currentTimeMillis() - t) < 3000L) {}
    2 + 4
  }

  val tmp = (for {
    s <- f5
    y <- f6
  } yield s * y) flatMap (x => Future(x))
  println("tmp value: " + tmp.value)

}
