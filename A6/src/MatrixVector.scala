import java.text.DecimalFormat
import scala.collection.SeqView
import scala.util.Random

/**
 * Created by sacry on 24/04/14.
 */

object StrassenMultiplication {

  def partition4(matrix: Vector[Double]): Vector[Vector[Double]] = {
    val n = math.sqrt(matrix.size).toInt
    lazy val m = matrix.view.sliding(n, n).toVector
    val n_2 = n / 2
    val tlMatrix = m.view(0, n_2).flatMap(p => p.view(0, n_2))
    val trMatrix = m.view(0, n_2).flatMap(p => p.view(n_2, n))
    val blMatrix = m.view(n_2, n).flatMap(p => p.view(0, n_2))
    val brMatrix = m.view(n_2, n).flatMap(p => p.view(n_2, n))
    Vector(tlMatrix, trMatrix, blMatrix, brMatrix).map(_.toVector)
  }

  def merge4(m: Vector[Vector[Double]]): Vector[Double] = {
    lazy val m11 = m(0)
    lazy val m12 = m(1)
    lazy val m21 = m(2)
    lazy val m22 = m(3)
    val n_2 = math.sqrt(m(0).size).toInt
    lazy val leftTop: Iterator[Vector[Double]] = m11.sliding(n_2, n_2)
    lazy val rightTop = m12.sliding(n_2, n_2)
    lazy val leftBot = m21.sliding(n_2, n_2)
    lazy val rightBot = m22.sliding(n_2, n_2)
    lazy val flat = leftTop.zip(rightTop).map {
      case (l, r) => l ++ r
    }.flatten ++
      leftBot.zip(rightBot).map {
        case (l, r) => l ++ r
      }.flatten
    flat.toVector
  }

  def add(v1: Vector[Double], v2: Vector[Double]) = {
   v1.zip(v2).map{case (l, r) => l + r}
  }

  def sub(v1: Vector[Double], v2: Vector[Double]) = {
    v1.zip(v2).map{case (l, r) => l - r}
  }

  def strassen(v1: Vector[Double], v2: Vector[Double]): Vector[Double] = {
    if (v1.size == 1 && v2.size == 1) {
      return Vector(v1(0) * v2(0))
    }
    lazy val a = partition4(v1)
    lazy val b = partition4(v2)

    val _11 = 0
    val _12 = 1
    val _21 = 2
    val _22 = 3

    lazy val m1 = strassen(add(a(_11), a(_22)), add(b(_11), b(_22)))
    lazy val m2 = strassen(add(a(_21), a(_22)), b(_11))
    lazy val m3 = strassen(a(_11), sub(b(_12), b(_22)))
    lazy val m4 = strassen(a(_22), sub(b(_21), b(_11)))
    lazy val m5 = strassen(add(a(_11), a(_12)), b(_22))
    lazy val m6 = strassen(sub(a(_21), a(_11)), add(b(_11), b(_12)))
    lazy val m7 = strassen(sub(a(_12), a(_22)), add(b(_21), b(_22)))

    lazy val c11 = add(sub(add(m1, m4), m5), m7)
    lazy val c12 = add(m3, m5)
    lazy val c21 = add(m2, m4)
    lazy val c22 = add(add(sub(m1, m2), m3), m6)

    merge4(Vector(c11, c12, c21, c22))
  }

}

object ForEachVector {
  def forEachGen[A](f: (Int => Int => A), n: Int): Vector[A] = {
    (for (i <- (0 until n); j <- (0 until n)) yield f(i)(j)).toVector
  }

  def forEach(f: (Int => Int => Double), n: Int): Vector[Double] = {
    (for (i <- (0 until n); j <- (0 until n)) yield f(i)(j)).toVector
  }
}

class MatrixVector private(val matrix: Vector[Double], val n: Int) {

  import ForEachVector._
  import StrassenMultiplication.strassen

  def apply(i: Int, j: Int) = matrix(i * n + j)

  def update(i: Int, j: Int, value: Double) =
    new MatrixVector(matrix.updated(i * n + j, value), n)

  lazy val size = n * n

  def sameSizeOf(m: MatrixVector) = m.n == n

  def +(m1: MatrixVector) = {
    require(sameSizeOf(m1))
    new MatrixVector(forEach(i => j => m1(i, j) + this(i, j), n), n)
  }

  def *(x: Double) = new MatrixVector(forEach(i => j => x * this(i, j), n).toVector, n)

  // https://en.wikipedia.org/wiki/Matrix_multiplication#Matrix_product_.28two_matrices.29
  // (A*B)(i)(j) == sum {k, 1, n, A(i)(k) * B(k)(j)
  def *(other: MatrixVector) = {
    require(sameSizeOf(other))
    new MatrixVector(
      forEach(i => j => (0 until n).map(k => this(i, k) * other(k, j)).sum, n).toVector
      , n)
  }

  def ≈∞(m1: MatrixVector) =
    new MatrixVector(strassen(this.matrix, m1.matrix), n)

  def -(m1: MatrixVector) = new MatrixVector(forEach(i => j => this(i, j) - m1(i, j), n), n)

  def unary_- = new MatrixVector(this.matrix.map(-_), n)

  override def equals(any: Any) = {
    lazy val other = any.asInstanceOf[MatrixVector]
    any.isInstanceOf[MatrixVector] && sameSizeOf(other) &&
      forEachGen(i => j => other(i, j) == this(i, j), n).reduceLeft(_ && _)
  }

  override def toString = {
    ("-" * 40) + "\n" + matrix.sliding(n, n).map(row => row.map("%.3f".format(_)).mkString(", ")).mkString("\n")
  }

}

object MatrixVector {

  import ForEachVector.forEach;

  def apply(x: Double*) = {
    if (math.sqrt(x.size).isWhole)
      new MatrixVector(x.toVector, math.sqrt(x.size).toInt)
    else
      throw new RuntimeException("DIE!")
  }

  def apply(xs: Vector[Vector[Double]]) =
    if (xs.forall(x => x.size == xs.size))
      new MatrixVector(xs.flatten, xs.size)
    else
      throw new RuntimeException("DIE!")

  def fromVector(x: Vector[Double]) =
    if (math.sqrt(x.size).isWhole)
      new MatrixVector(x, math.sqrt(x.size).toInt)
    else
      throw new RuntimeException("DIE!")

  def apply(n: Int, optional: Double = 0.0) = new MatrixVector(Vector.fill(n * n)(optional), n)

  def apply(n: Int, f: ((Double, Double) => Double)) = new MatrixVector(
    forEach(i => j => f(i, j), n), n
  )

  def apply(n: Int, t: Triple[Double, Double, Double]*) =
    new MatrixVector(
      (for (i <- (0 until n);
            j <- (0 until n);
            (i1, j1, k) <- t;
            if i == i1 && j1 == j)
      yield k).toVector, n
    )

  def Zero(n: Int) = MatrixVector(n, 0.0)

  def filledVector(d: Int, maxVal: Double) = {
    Vector.fill(d, d) {
      maxVal * new Random().nextDouble()
    }
  }

  def main(args: Array[String]) {
    val mat1 = MatrixVector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0)
    val fuck = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0)
    //println(fromVector(strassen(fuck, fuck)))
    println(mat1 * mat1)

  }

}