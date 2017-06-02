package tutorial.arun

import util.Random.nextInt



/**
  * @author arunvarma.
  */
object SortExample {

  def main(args: Array[String]): Unit = {

    var myList = Stream.continually(nextInt(1000000)).take(100000).toArray

    println("Unsorted Array")

    println(myList.mkString(" "))

    val start: Long  = System.currentTimeMillis()

    sort(myList)

    val end: Long = System.currentTimeMillis()

    val diff = end - start

    println("After Sorting")

    println(myList.mkString(" "))

    println("Time taken: " + diff)

  }


  /**
    * sorting of an array
    * @param xs array of integer
    */
  def sort(xs: Array[Int]): Unit = {

    def swap(i: Int, j: Int): Unit = {
      val t = xs(i)
      xs(i) = xs(j)
      xs(j) = t
    }

    def sort1(l: Int, r: Int): Unit = {
      val pivot = xs((l + r) / 2)

      var i = l
      var j = r

      while (i <= j) {

        while (xs(i) < pivot) i += 1
        while (xs(j) > pivot) j -= 1

        if (i <= j) {
          swap(i, j)
          i += 1
          j -= 1
        }
      }

      if (l < j) sort1(l, j)
      if (j < r) sort1(i, r)
    }

    sort1(0, xs.length - 1)
  }
}
