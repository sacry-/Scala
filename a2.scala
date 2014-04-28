import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

// Collatz
// Tail Recursive Optimized Array Version..
def collatz_arr(n: Int): Array[Int] = {
  @tailrec def collatz_tail(x: Int, accu: Array[Int]): Array[Int] = x match {
    case 1 => accu :+ 1 // O(?)
    case x if x % 2 == 0 => collatz_tail(x / 2, accu :+ x)
    case _ => collatz_tail(3 * x + 1, accu :+ x)
  }
  collatz_tail(n, Array())
}

def collatz_arr_short(n: Int): Array[Int] = {
  @tailrec def collatz_tail(x: Int, accu: Array[Int]): Array[Int] = x match {
    case 1 => accu :+ 1 // O(n)
    case _ => collatz_tail(if (x % 2 == 0) x / 2 else 3 * x + 1, accu :+ x)
  }
  collatz_tail(n, Array())
}

// Tail Recursive Optimized ArrayBuffer Version..
def collatz_arr_buffer(n: Int): Array[Int] = {
  @tailrec def collatz_tail(x: Int, accu: ArrayBuffer[Int]): ArrayBuffer[Int] = x match {
    case 1 => accu += 1 // O(1)
    case x if x % 2 == 0 => collatz_tail(x / 2, accu += x)
    case _ => collatz_tail(3 * x + 1, accu += x)
  }
  collatz_tail(n, ArrayBuffer()).toArray
}

def collatz_arr_buffer_short(n: Int): Array[Int] = {
  @tailrec def collatz_tail(x: Int, accu: ArrayBuffer[Int]): ArrayBuffer[Int] = x match {
    case 1 => accu += 1 // O(1)
    case _ => collatz_tail(if (x % 2 == 0) x / 2 else 3 * x + 1, accu += x)
  }
  collatz_tail(n, ArrayBuffer()).toArray
}

// Simple List Recursion
def collatz_rec(n: Int): List[Int] = n match {
    case 1 => List(1)
    case n if n % 2 == 0 => n :: collatz_rec(n / 2)
    case _ => n :: collatz_rec(3 * x + 1)
  }
}

// A1
abstract class Abstract {
  val name: String
  var i: Int
  def nc = name.charAt(i)
  def f[T, R](i: T): R
  override def toString = "(" + name + ", " + i + ", " + nc + ", " + f _ + ")"
}

class Concrete extends Abstract {
  val name = "Welt"
  var i = 0
  def f[T, R](i: T): R = f(i)
  override def toString = "Concrete" + super.toString
}

object Test {

  def main(args: Array[String]) {
    println(new Concrete)
  }
}

// A2
trait Shape {
  def basePoint: Point
}
trait ClosedShape extends Shape {
  def area: Double
}

case class Point(x: Int, y: Int)
case class Line(xy: Point, yy: Point) extends Shape {
  val basePoint = xy
}
case class Circle(xy: Point, d: Int) extends ClosedShape {
  override def toString = "circle mit r " + (if (d >= 5) ">=" else "<") + " 5"
  val basePoint = xy;
  val area = math.Pi * math.pow(d, 2)
}

// maximum Supertype allowed is Shape and T is Contravariant
class UseShape[+T <: Shape](t: T) {
  override def toString = "basepoint at: " + t.basePoint + " -> " +
    (t match {
      case t: Circle => t.toString
      case n: ClosedShape => "unbekanntes closed shape mit area = " + n.area
      case _ => "unbekanntes shape"
    })
}

object Test2 {
  def main(args: Array[String]) {
    test01
  }

  def useAllShapes(u: UseShape[Shape]) {
    println(u)
  }

  def test01 = {
    val useLine = new UseShape(Line(Point(0, 0), Point(1, 1)))
    val useCircle1 = new UseShape(Circle(Point(1, 2), 10))
    val useCircle2 = new UseShape(Circle(Point(2, 1), 4))

    val useClosedShape = new UseShape(
      new ClosedShape {
        def basePoint: Point = Point(1, 1);
        def area: Double = 10D
      })

    useAllShapes(new UseShape(
      new ClosedShape {
        def basePoint: Point = Point(2, 2);
        def area: Double = 20D
      }))

    useAllShapes(useClosedShape)
    useAllShapes(useLine)
    useAllShapes(useCircle1)
    useAllShapes(useCircle2)
  }

}

