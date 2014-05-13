import scala.util.Random

/**
 * Created by sacry on 05/05/14.
 */
object Environment {

  import Matrix._
  import MatrixVector._
  import java.util.concurrent._

  def intG(from: Int, to: Int) = from + new Random().nextInt(math.abs(from - to + 1))

  def doubleG(from: Int, to: Int) = intG(from, to) * new Random().nextDouble()

  def printOS {
    import System.{getProperty => prop}

    println("# cores : %d".format(Runtime.getRuntime.availableProcessors))
    println("# OS : %s %s (%s)".format(
      prop("os.name", "unknown os"),
      prop("os.version", "<unknown version>"),
      prop("os.arch", "UNKNOWN")
    ))
    println("# JVM : " + prop("java.version", "unknown"))
  }

  def execTime[A](body: => A) = {
    printOS
    val t0 = System.nanoTime
    val result = body
    val t = System.nanoTime - t0

    //println(result)

    val m = result.asInstanceOf[MatrixVector]
    val s = " for: " + m.n + "x" + m.n + " = " + m.size + " elements\n"
    println("~%.3fmsec".format((t / 1000L).toDouble / 1000d) + s)
    result
  }

  def filledVector(d: Int, maxVal: Double) = {
    Vector.fill(d, d) {
      maxVal * new Random().nextDouble()
    }
  }

  def main(args: Array[String]) {
    val d = 64
    val mat1 = MatrixVector(filledVector(d, 1000.0))
    val mat2 = MatrixVector(filledVector(d, 1000.0))
    execTime(mat1 ≈∞ mat2)
    execTime(mat1 ≈∞ mat2)
    val d1 = 256
    val mat3 = MatrixVector(filledVector(d1, 1000.0))
    val mat4 = MatrixVector(filledVector(d1, 1000.0))
    execTime(mat3 ≈∞ mat4)
    //execTime(mat1 * mat2)
  }

}