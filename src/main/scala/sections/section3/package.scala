package sections

package object section3 {
  def findNemo(array: Array[String]): Int = {
    val values = for {
      i <- array.indices if array(i) == "nemo"
    } yield {
      1
    }

    values.sum
  }


  def logAllPairsOfArray(array: Array[String]): Unit = {
    for {
      i <- array.indices
      j <- Range(i, array.length)
    } yield {
      println(s"${array(i)}, ${array(j)}")
    }
  }

}

object Section3App extends App {
  import section3.logAllPairsOfArray

  logAllPairsOfArray(Array("a", "b", "c"))
}
