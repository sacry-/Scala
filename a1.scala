    
// 1.0
val ls = List()
1 :: ls
's' :: ls
2L :: ls
2.0f :: ls
() :: ls
3.0d :: ls
"Hallo" :: ls

/*
1.1 Scala hat eine eigene Typ-Hierarchie die Prezendenzen untereinander vorweisen
Jeder Typ, egal welcher geht zu einem Typen namens Scala Any. Alle grundlegen Typen 
wie int etc sind abstrakter ein AnyVal. All List, Sequenzen auch String typen sind 
eine AnyRef (also eine Referenz). Zugrunde liegt außerdem ein Typ Nothing. Jeder Typ 
in Scala kann Nothing sein inklusive des null typens. Über generics, wenn kein Typ 
definiert wird einfach der größte tTyp "any" genommen den man in eine Liste packen kann.

1.2 
(a) Immutable weil eine Liste nicht veränderbar sein soll. Die Veränderung einer Liste 
führt zwangsweise immer zu einer neuen Liste.
(b) Kovarianz heißt, dass ein gegebener Typ von einem Generischen Supertypen überführt werden kann.
Somit ist Liste zuerst vom Typ Int und dann vom Typ AnyVal, weil Char und Int diesen generischen
Typen gemeinsam haben. Ab dem Typen String ist die List vom Typ Any List[Any], was erlaubt ist
da es ein generischer typ zu den bisherigen ist der diese verbindet.
(c) Durch Immutability ist es von vorteil persistente Datentypen zu haben, das heißt Datentypen auf die man
schnell wieder zugreifen kann. Wenn bei jedem hinzufügen zu einer Liste immer eine komplett neue Liste entstehen
würde ist dies nicht optimal. Das hinzufügen am Head, der Zugriff auf den Tail und das entfernen des Head sollten immer
O(1) sein. Dies geht nur wenn man beispielweise wie bei Github so etwas wie eine commit Hierarchie anlegt für jedes
Element das hinzugefügt oder entfernt wird. Damit man schnell zu einer vorherigen Liste kommt muss man schlichtweg 
den commit reverten O(1).
*/

// 2.2

// (a) 3..19
val xs = 3 to 19 toArray

// (b) slice it
xs.slice(3, 11)

// Python like slicing... for phuns..
object Slicer {
  implicit class FancyList[A](val l: List[A]) extends AnyVal {
    def ~>(i: Int, j: Int, k: Int = 1) = {
      l.zipWithIndex
        .slice(normalize(i), normalize(j))
        .filter(_._2 % k == 0)
        .map(_._1)
    }
    private def normalize(j: Int) = if (j < 0) l.size + 1 + j else j
  }
  val xs = ((1 to 19) toList)
  println(xs)
  println("-----1------")
  println(xs ~> (1, 10))
  println(xs ~> (0, -1))
  println(xs ~> (0, -10))
  println("-----2------")
  println(xs ~> (1, 10, 2))
  println(xs ~> (0, -1, 2))
  println(xs ~> (0, -10, 2))
  println("-----3------")
  println(xs ~> (0, -1, 3))
  println(xs ~> (-14, -10, 1))
  println(xs ~> (-14, -10, 2))
}


// (c) only mod 3s
xs filter(_ % 3 == 0)

// (d) intermediate sum
xs.scanLeft(0) { _+_ }
// or
// def add[T](a: T, b: T)(implicit n: Numeric[T]): T = n.plus(a, b)
def add(a: Int, b: Int) = a + b
xs.scanLeft(0)(add)

// (e) elem, index from idx 10 to -1
xs.zipWithIndex.drop(10)
// or..
for(t <- xs.zipWithIndex if t._2 > 9) yield t

// (f) with swap self defined
def swap[A, B](t: (A, B)) = (t._2, t._1)
xs.zipWithIndex.drop(10).map(swap)
// with build in swap
xs.zipWithIndex.drop(10).map(_.swap)
// without swap
for((e, i) <- xs.zipWithIndex if i > 9) yield (i, e)

     