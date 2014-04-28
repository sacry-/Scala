import scala.util.Random
import scala.annotation.tailrec

/**
 * Created by sacry on 01/04/14.
 */

object StringGenerator {
  def intG(from: Int, to: Int) = from + Random.nextInt(math.abs(from - to + 1))

  def intGToChar(from: Int, to: Int) = intG(from, to).asInstanceOf[Char]

  def rdmRange(min: Int, max: Int) = 0 to intG(min, max)

  def generate(min: Int, max: Int, from: Int, to: Int) = (for (e <- rdmRange(min, max)) yield intGToChar(from, to)).mkString("")

  def gFold(min: Int, max: Int, from: Int, to: Int) = rdmRange(min, max).foldLeft("")((a, _) => a + intGToChar(from, to))

  def main(args: Array[String]) {
    println(StringGenerator.generate(min = 5, max = 15, from = 0, to = 128))
    println(StringGenerator.gFold(min = 5, max = 15, from = 0, to = 128))
    println(StringGenerator.gFold(5, 15, 60, 90))
  }
}

object sumProds {

  def sumOfProds(l1: List[Int], l2: List[Int]) = {
    l1.zip(l2).foldLeft(0)((accu, t) => accu + (t._1 * t._2))
  }

  def sumOfProds2(l1: List[Int], l2: List[Int]) = {
    (1 to math.min(l1.size, l2.size))
      .foldLeft((l1, l2, 0))(
        (t3, _) => (t3._1.tail, t3._2.tail, t3._3 + (t3._1.head * t3._2.head))
      )._3
  }

  def sumOfProds3(l1: List[Int], l2: List[Int]) = {
    ZipFold.zipFoldLeft(l1, l2, 0)(_ * _ + _)
  }

  def product(l1: List[Int], l2: List[Int]) = {
    l1.zip(l2).map(t => t._1 * t._2)
  }

  def main(args: Array[String]) {
    val c = sumOfProds(List(1, 2, 3, 4), List(1, 2, 3, 4))
    println(c)
    val c2 = sumOfProds2(List(1, 2, 3), List(1, 2, 3, 4, 4))
    println(c2)
    val c3 = sumOfProds3(List(1, 2, 3), List(1, 2, 3, 4, 4))
    println(c3)
    val d = product(List(1, 2, 3), List(1, 2, 3, 4))
    println(d)
  }

}

case class Person private(val name: String, var age: Int = 0, private val pw: String = StringGenerator.gFold(5, 15, 65, 90)) {
  override def toString = "Person(" + name + ", " + age + ")"
}

object Person {
  def main(args: Array[String]) {
    val p = Person("matze", 26)
    println(p)
    println(p.pw)
  }
}

class User(val name: String)

trait AnyMail {
  def provider = "<domain>.<country>"

  def user: String

  def address = "any" + "@?" + provider
}

trait GMail extends AnyMail {
  override val provider = "gmail.com"
  override val user: String

  override def address = user + "@" + provider
}

trait Bank {
  def account: Long
}

case class Costumer(override val name: String, override val account: Long) extends User(name) with GMail with Bank {
  val user = name
  override val address = super.address

  override def toString = "Costumer(" + user + ", " + address + ", " + account + ")"
}

object CostumerTest {
  def main(args: Array[String]) {
    val c = Costumer("Matthias", 1234567L)
    println(c)
  }
}


object ZipFold {
  // Types T and A shall be different for the Lists
  // A Function f produces an Accumulator of Type U over Type T and A
  @tailrec def zipFoldLeft[T, A, U](l1: List[T], l2: List[A], acc: U)(f: (T, A, U) => U): U = {
    if (l1 == Nil || l2 == Nil)
      acc
    else
      zipFoldLeft(l1.tail, l2.tail, f(l1.head, l2.head, acc))(f)
  }

  // Proper Naming for multi Zipping...
  @tailrec def zip2FoldLeft[T, A, U](l1: List[T], l2: List[A], acc: U)(f: (T, A, U) => U): U = {
    if (l1 == Nil || l2 == Nil)
      acc
    else
      zip2FoldLeft(l1.tail, l2.tail, f(l1.head, l2.head, acc))(f)
  }

  // Proper Naming for multi Zipping...
  @tailrec def zip3FoldLeft[T, A, B, U](l1: List[T], l2: List[A], l3: List[B], acc: U)(f: (T, A, B, U) => U): U = {
    if (l1 == Nil || l2 == Nil || l3 == Nil)
      acc
    else
      zip3FoldLeft(l1.tail, l2.tail, l3.tail, f(l1.head, l2.head, l3.head, acc))(f)
  }
}

object ZipFoldTest {

  // Object Functional
  implicit class FancyList[T](val l1: List[T]) extends AnyVal {
    def zipFoldLeft[A, U](l2: List[A], acc: U)(f: (T, A, U) => U): U = {
      if (l1 == Nil || l2 == Nil)
        acc
      else
        l1.tail.zipFoldLeft(l2.tail, f(l1.head, l2.head, acc))(f)
    }
  }

  def main(args: Array[String]) {
    val l1 = List(1, 2, 3, 4, 5)
    val l2 = List(1, 2, 3, 4, 5)
    val l3 = List(1, 2, 3, 4, 5)
    val res1 = ZipFold.zipFoldLeft(l1, l2, 0)((a, b, acc) => acc + (a * b))
    println(res1)
    // Little more verbose? ;))
    val res2 = ZipFold.zip2FoldLeft(l1, l2, 0)(_ * _ + _)
    println(res2)
    val res3 = ZipFold.zip3FoldLeft(l1, l2, l3, 0)(_ * _ * _ + _)
    println(res3)
    val res4 = l1.zipFoldLeft(l2, 0)((a, b, acc) => acc + (a * b))
    println(res4)
  }
}
