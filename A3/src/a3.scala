import scala.annotation.tailrec

/**
 * Created by sacry on 05/04/14.
 */


case class Polynom private(cs: List[Pair[Int, Int]]) {
  def apply(x: Int) = cs.foldLeft(0)((acc, c) => acc + (c._1 * (x ^ c._2)))

  def +(p: Polynom): Polynom = {
    Polynom(
      (this.cs ++ p.cs)
        .groupBy(_._2)
        .mapValues(_.unzip._1.sum)
        .toList
        .map(_.swap)
    )
  }

  def *(p: Polynom): Polynom = {
    Polynom(for {
      (c1, exp1) <- cs
      (c2, exp2) <- p.cs
      (c, exp) = (c1 * c2, exp1 + exp2)
    } yield (c, exp)
    )
  }

  def °(p: Polynom): Polynom = {
    this.cs.foldLeft(Polynom.ZERO)((acc, t) => Polynom(t) + p * acc)
  }

  def ^(exp: Int): Polynom = exp match {
    case 0 => Polynom.ONE
    case 2 => this * this
    case x if (x % 2 == 0) => (this ^ (x / 2)) ^ 2
    case x => (this ^ (x - 1)) * this
  }

  override def toString = "Polynom( " +
    (if (this == Polynom.ZERO) {
      "0"
    } else if (this == Polynom.ONE) {
      "1"
    } else {
      this.cs.filter(p => p._1 != 0).sortBy(p => -p._2).foldLeft(("", 0)) {
        (acc, b) => (acc._1 + (if (acc._2 > 0) " + " else "") + b._1 + "x^" + b._2, acc._2 + 1)
      }._1
    }) + " )"
}

object Polynom {

  def apply(cFirst: Int, csRest: Int*) = {
    val cs = cFirst :: csRest.toList
    new Polynom(cs.reverse.zipWithIndex)
  }

  def apply(cFirst: Pair[Int, Int], cs: Pair[Int, Int]*) = new Polynom(cFirst :: cs.toList)

  lazy val ZERO: Polynom = Polynom(0)
  lazy val ONE: Polynom = Polynom(1)
}

object Test {
  def main(args: Array[String]) {
    val zero = Polynom(0)
    val one = Polynom(0, 0, 0, 1)
    val p1 = Polynom(0, 0, 0, 1, 1)
    val p2 = Polynom(4, 3, 2, 1)
    val p3 = Polynom(3, 0, 5)
    val p4 = Polynom(-1, 1, -10)
    val p5 = Polynom((10, 50))

    println(zero + ", " + one)
    println(zero ° p2)
    println(p2 ° zero)
    println(p2 ° p3)
    println(p2 * p3)
    println((p2 ° p3)(-1))
    println((p2 * p3)(-1))
    println(p1 * p2)
    println(p3 * p4)
    println(p1 + " + " + p2 + " = " + (p1 + p2))
    println((p1 + p2)(1))
    println((p1 ° p2) + "(1)= " + (p1 ° p2)(1))
    println((p2 ° p1) + "(1)= " + (p2 ° p1)(1))
    println((p1 * p2)(1))
    println((p2 * p1)(1))
    println(p5)
  }
}
