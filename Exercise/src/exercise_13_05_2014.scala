/**
 * Created by sacry on 13/05/14.
 */

abstract class Buffer {
  type T
  val elem: T
}

case class ConcreteBuffer(val xs: List[Int]) extends Buffer {
  type T = Int
  val elem = xs.head
}

object exercise_13_05_2014 extends App{
  println(ConcreteBuffer(List(1)))
}
