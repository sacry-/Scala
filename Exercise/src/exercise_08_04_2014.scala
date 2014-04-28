/**
 * Created by sacry on 08/04/14.
 */

object Boring {

  def line(a: Double, b: Double)(f: (Double, Double) => Double)
  = (x: Double) => (y: Double)
  => (f2: Double => Double)
    => f2(f(a, b) + x * y)

  def n(x: Double) = (-x)

  def mul(x: Double) = (y: Double) => x * y

  def i(x: Double) = x + 1

  def c(f: (Double => Double)*) = (x: Double) => {
    f.foldLeft(x)((acc, func) => func(acc))
  }

  def main(args: Array[String]) {
    lazy val res = line(2.0, 3.0)(_ + _ * 3.0)(3.0)(5.0)(_ * 2.0)
    println(res)

    lazy val sqr_neg = c(n, i, n, i, n)
    println(sqr_neg(4.0))

    val some_func = (x: Double) => x * 2
    some_func(4.0)
    
    println(mul(2.0)(2.0))
  }

}

