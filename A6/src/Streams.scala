import scala.collection.immutable.Stream.{Empty, cons}

/**
 * Created by sacry on 28/04/14.
 */

object StreamsTest {

  def triangleSeq1: Stream[Int] = {
    def triAcc(n: Int = 1): Stream[Int] = cons(n * (n + 1) / 2, triAcc(n + 1));
    triAcc()
  }

  def triangleSeq2: Stream[Int] = 1 #:: 3 #:: triangleSeq2.zip(triangleSeq2.tail).map {
    case (a, b) => b + (b - a) + 1
  }

  def triangleSeq3 = Stream.iterate((0, 1)) {
    case (a, b) => (b, b + (b - a) + 1)
  }.map(t => t._2).init

  def triangleSeq4 = Stream.from(1).map(c => c * (c + 1) / 2)

  def headStream[A](s: Stream[A]): Stream[Stream[A]] = s match {
    case Empty => Empty
    case hd #:: tl => (Empty #:: headStream(tl)).map(hd #:: _)
  }

  def main(args: Array[String]) {
    triangleSeq1.take(7).foreach(println)
    triangleSeq2.take(7).foreach(println)
    triangleSeq3.take(7).foreach(println)
    triangleSeq4.take(7).foreach(println)
    headStream(Stream.from(10).take(5)).foreach(s => println(s.force))
  }
}