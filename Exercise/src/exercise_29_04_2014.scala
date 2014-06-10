import com.apple.jobjc.NativeObjectLifecycleManager.Nothing

/**
 * Created by sacry on 29/04/14.
 */

class Zomfg private(val x: Int) {
  fu =>
  def apply(y: Int) = fu.zomfg(y)

  case class zomfg(val x: Int) {
    self =>
    def swaneetStinkt = self.x
  }

  def swaneetStinkt = this.x

}

object Zomfg {

  def apply(x: Int) = new Zomfg(x)

  type ~>[-T, +R] = PartialFunction[T, R]

  def main(args: Array[String]) {
    println(Zomfg(1)(2).swaneetStinkt)
  }
}

object exercise_29_04_2014 {

  type ~>[-T, +R] = PartialFunction[T, R]

  implicit class FancyList[A](xs: List[A]) {
    def headOrDefault(default: => A) = if (xs.isEmpty) default else xs.head

    def headOrElse = xs match {
      case Nil => None
      case (x :: _) => Some(x)
    }
  }

  def main(args: Array[String]) {
    val res1 = List[Int]().headOrDefault(default = 1)
    val res2 = List(2).headOrDefault(default = 1)
    val res_maybe1 = List(2).headOrElse
    val res_maybe2 = List().headOrElse
    println(res1)
    println(res2)
    println(res_maybe1)
    println(res_maybe2)
  }
}


