package SS_13

import scala.util.{Success, Failure, Random, Try}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.actors.Actor

/**
 * Created by sacry on 25/06/14.
 */
object Main extends App {
  // 1
  def readValue = {
    val i = math.abs(Random.nextInt(10))
    if (i % 3 == 0) throw new Exception(i + " ungültig") else i
  }

  // 1a
  val lst = (1 to 10).foldLeft(List[Try[Int]]()) {
    (acc, _) => Try(readValue) :: acc
  }
  println(lst)
  // 1b
  println(lst.filter(_.isFailure))
  // 1c
  println(lst.map(_.getOrElse(())))
  // 1d
  val plain = lst.foldLeft(List[Int]()) {
    case (acc, Success(elem)) => elem :: acc
    case (acc, Failure(elem)) => acc
  }
  println(plain)

  // 2
  def distinctPairs(xs: List[Int]): List[(Int, Int)] = {
    assert(xs.size > 1)
    for {x <- xs
         y <- xs
         if x != y} yield (x, y)
  }

  println(distinctPairs(List(1, 2, 3)))
  println(distinctPairs(List(3, 5, 7, 9)))

  // println(distinctPairs(List(3)))

  // 3
  trait PreciseNum[T] {
    def isValidLong(num: T): Boolean
  }

  object PreciseNum {

    implicit object LongMonoid extends PreciseNum[Long] {
      def isValidLong(num: Long) = true
    }

    implicit object BigDecimalMonoid extends PreciseNum[BigDecimal] {
      def isValidLong(num: BigDecimal) = num.isValidLong
    }

  }

  def usePreciseNum[T: PreciseNum](num: T): Boolean = {
    val m = implicitly[PreciseNum[T]]
    m.isValidLong(num)
  }

  println(usePreciseNum(BigDecimal(10.0)))
  println(usePreciseNum(10L))
  println(usePreciseNum(BigDecimal(10.1)))

  // 4 - some things open!
  trait Account

  case class SAccount(id: Int, balance: Double) extends Account

  case class BAccount(id: Int, balance: Double)

  case class Bookings[A <: Account](account: A, bookings: List[Double])

  val a = SAccount(1, 1000.0)
  val b = Bookings(a, List(10.0, 100.0))
  val b2 = Bookings(new Account {}, List(10.0, 100.0))
  val a2 = BAccount(1, 1000.0)
  // val b3 = Bookings(a2, List(10.0,100.0))

  // 5

  import scala.concurrent._
  import ExecutionContext.Implicits.global

  val t = System.nanoTime
  val f1 = Future {
    Thread.sleep(2000L)
    10
  }
  val f2 = Future {
    Thread.sleep(3000L)
    30
  }
  val r1 = Await.result(f1, Duration.Inf)
  val r2 = Await.result(f2, Duration.Inf)
  val t2 = System.nanoTime
  println(r1 + r2)
  println(t2 - t)

  // 6

}