import scala.util.Random

/**
 * Created by sacry on 22/04/14.
 */
object exercise_22_04_2014 {

  def printStream(stream: Stream[Int]) = {
    for (elem <- stream)
      print(elem)
  }

  def main(args: Array[String]) {
    lazy val randomBinaries = (i: Int) => Stream.continually(Random.nextInt(2)).take(i)
    def randomBinaries2: Stream[Int] = Stream.cons(Random.nextInt(2), randomBinaries2)

    printStream(randomBinaries2.take(20))
    val res = 2 ^ (4)
    println(math.pow(2.0, 4.0))
  }
}
