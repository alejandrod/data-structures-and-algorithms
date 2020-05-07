package sections.section7

import scala.collection.mutable

object IQ83FirstRecurringElement {


  def firstRecurringElement[T](arrayA: Array[T]): Option[T] = {
    val map = mutable.HashSet.empty[T]
    arrayA.find(e => {
      if (map.contains(e)) {
        true
      } else {
        map.add(e)
        false
      }
    })
  }

}

object IQ83FirstRecurringElementApp extends App {

  import IQ83FirstRecurringElement._


  assert(firstRecurringElement(Array(2, 5, 1, 2, 3, 5, 1, 2, 4)) == Option(2))
  assert(firstRecurringElement(Array(2, 1, 1, 2, 3, 5, 1, 2, 4)) == Option(1))
  assert(firstRecurringElement(Array(2, 3, 4, 5)).isEmpty)
  assert(firstRecurringElement(Array.empty).isEmpty)
}
