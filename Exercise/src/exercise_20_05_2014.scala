import com.sun.xml.internal.messaging.saaj.packaging.mime.internet.ParameterList
import scala.collection.parallel.immutable.ParVector

/**
 * Created by sacry on 20/05/14.
 */

object MonadStuff extends App {

}

object test extends App{
  val r = (1 to 100000000)
  for(i <- r if i%1000000 == 0) print(i + " ")
}

object ParallelCollections extends App {
  val l: Vector[Int] = (1 to 1000000).toVector
  var i = 0L
  var acc = 0L
  while (i < 300L) {
    val t1 = System.currentTimeMillis();
    l.map(_ + i)
    val t2 = System.currentTimeMillis();
    i += 1
    acc += (t2 - t1)
  }
  println(acc)
  val v: ParVector[Int] = (1 to 1000000).toVector.par
  var i2 = 0L
  var acc2 = 0L
  while (i2 < 300L) {
    val t1 = System.currentTimeMillis();
    v.map(_ + i)
    val t2 = System.currentTimeMillis();
    i2 += 1
    acc2 += (t2 - t1)
  }
  println(acc2)
}