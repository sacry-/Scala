import scala.util.{Try, Success, Failure}

/**
 * Created by sacry on 08/05/14.
 */
object A8 {

  // 1
  def sequenceOption[A](lst: List[Option[A]]): Option[List[A]] = {
    Some(for {
      e <- lst
    } yield e getOrElse (return None))
  }

  // 2
  case class Person(id: String)

  case class Company(ps: Person)

  val customers = List(Left(Person("P1")), Left(Person("P2")), Right(Company(Person("CEO1"))), Left(Person("P3")),
    Right(Company(Person("CEO2"))), Right(Company(Person("CEO3"))), Left(Person("P3")))

  // 3
  trait ~>[A, B] extends (A => B) {
    def applyOrDefault(x: A, default: B): B
  }

  def use1[A, B](f: A => B)(x: A): B = f(x)

  def use2[A, B](f: A ~> B)(x: A)(default: B): B = f.applyOrDefault(x, default)

  val f: Double ~> Double = new (Double => Double) with (Double ~> Double) {
    def applyOrDefault(x: Double, default: Double) = if (x >= 0) this.apply(x) else default

    def apply(x: Double) = math.sqrt(x)
  }

  // 4 - Just Functors -> The naming!
  trait TypeCons[F[_]] {
    def fromAToFB[A, B](fa: F[A])(f: A => B): F[B]
  }

  def listCons: TypeCons[List[_]] = new TypeCons[List] {
    def fromAToFB[A, B](fa: List[A])(f: A => B) = fa.map(f)
  }

  def optionCons: TypeCons[Option] = new TypeCons[Option] {
    def fromAToFB[A, B](m: Option[A])(f: A => B) = m.map(f)
  }

  def main(args: Array[String]) {
    // 1
    println(sequenceOption(List(Some(1), Some(2), None)))
    println(sequenceOption(List(Some("Hallo"), Some("Welt"))))

    // 2
    println(customers partition (_ isLeft))

    println(customers flatMap (v => if (v isLeft) Some(v) else None))
    println(customers flatMap {
      case Left(x) => Some(x);
      case _ => None
    })

    println(customers flatMap (v => if (v isRight) Some(v) else None))
    println(customers flatMap {
      case Right(x) => Some(x);
      case _ => None
    })

    // 3
    println(use1(f)(2))
    println(use1(f)(-2.0))
    println(use2(f)(0.0)(-2.0))

    // 4
    println(listCons.fromAToFB(List("Hello", "students", "!"))(_.size))
    val romanMap = Map("I" -> 1, "II" -> 2, "III" -> 3)
    println(optionCons.fromAToFB(romanMap.get("III"))(2 * _))
    println(optionCons.fromAToFB(romanMap.get("IIII"))((i: Int) => 2 * i))
  }

}
