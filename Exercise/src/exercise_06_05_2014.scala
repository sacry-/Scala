/**
 * Created by sacry on 06/05/14.
 */
object exercise_06_05_2014 {

  val dlist = List(Some(100), Some(10), None, Some(5), None, Some(150))

  def main(args: Array[String]){
    println(for(x <- dlist) yield x map(math.abs(_)) filter(_ < 100))
    for(x <- dlist)
      println(x map(math.abs(_)) filter(_ < 100) getOrElse(""))
  }



}
