package ws12_13

import scala.concurrent.{Await, ExecutionContext, Future}
import ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

/**
 * Created by sacry on 18/06/14.
 */

trait A {
  def foo = "A"
}

trait B extends A {
  override def foo = "B"
}

object WS_12_13 extends App {

  // 1
  def bar(a: A) {
    println(a.foo)
  }

  def test01 {
    bar(new A {}) // Instanz von A übergeben
    bar(new B {}) // Instanz von B übergeben
  }

  test01

  // 2
  val t = List(1, 2, 3).filter(_ % 2 == 0).flatMap(x => List(4, 5, 6).map(y => x * y))
  println(t)
  val r = for {x <- List(1, 2, 3)
               y <- List(4, 5, 6)
               if x % 2 == 0} yield x * y
  println(r)

  // 3
  val i: Option[Int] = Some(3)
  var j: Option[Int] = Some(4)
  println(for {
    x <- i
    y <- j
  } yield (x * y))
  j = None
  println((for {
    x <- i
    y <- j
  } yield (x * y)) getOrElse (-1))

  // 4
  val f = Future {
    5
  }
  val g = Future {
    3
  }
  val h = for {// Promise
    x <- f
    y <- g
  } yield x + y
  Await.ready(f, Duration.Inf)
  Await.ready(g, Duration.Inf)
  println(h.value)

  // Some(Success(8)) h.value.get.get

  // 5 Xor :(
  trait XOr[T] {
    def xor(that: T): T
  }

  case class XorInt(i: Int) extends XOr[Int] {
    def xor(that: Int) = i ^ that
  }

  case class XorString(s: String) extends XOr[String] {
    def xor(that: String) =
      s.zip(that).map {
        case (a, b) => a ^ b
      }.mkString("")
  }

  implicit def intToXOr(i: Int): XOr[Int] = XorInt(i)

  implicit def stringToXOr(s: String): XOr[String] = XorString(s)

  type WithXOr[T] = XOr[T]

  //def foo[T: XOr](a: T, b:T) = a xor b

  println(12 xor 4)
  println("123" xor "12")
  println("321" xor "123")

  // 6
  val sum: Int => (Int => (Int => Int)) = (a: Int) => (b: Int) => (c: Int) => a + b + c
  sum(1)(2)(3)
  println(sum(1))
  // <function1>
  println(sum(1)(2))

  // <function1>

  // 7
  def foo(t: {def close}) = t.close

  foo(new {
    def close = println("close")
  })

  // 8 Akka :(

}
