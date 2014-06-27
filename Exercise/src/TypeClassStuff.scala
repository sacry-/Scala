
import scala.util.Try

/**
 * Created by sacry on 25/06/14.
 */

object TypeClasses1 {

  trait PreciseNum[T] {
    def isValidLong(num: T): Boolean
  }

  object PreciseNum {

    implicit object LongMonoid extends PreciseNum[Long] {
      def isValidLong(num: Long) = true
    }

    implicit object BigDecimalMonoid extends PreciseNum[BigDecimal] {
      def isValidLong(num: BigDecimal) = num.isValidLong
    }

  }

  def usePreciseNum[T: PreciseNum](num: T): Boolean = {
    val m = implicitly[PreciseNum[T]]
    m.isValidLong(num)
  }
}

object TypeClassStuff {

}
