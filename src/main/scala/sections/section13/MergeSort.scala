package sections.section13

object MergeSort {

  // Since we split and merge arrays, making it in place will require more complicated logic.
  // Version 1 using recursion
  def sort[T <: Comparable[T]](array: Array[T])(implicit ord: Ordering[T]): Unit = {
    val sorted = functionalSort(array.toList)
    sorted.zipWithIndex.foreach {
      case (v, i) => array(i) = v
    }
  }

  def sortInplace[T <: Comparable[T]](array: Array[T])(implicit ord: Ordering[T]): Unit = {
    inplaceSort(array, 0, array.length - 1)
  }

  private def functionalSort[T <: Comparable[T]](seq: List[T])(implicit ord: Ordering[T]): List[T] = {
    val length = seq.length
    if (length > 1) {
      val middle = length / 2
      val (left, right) = seq.splitAt(middle)
      functionalMerge(functionalSort(left), functionalSort(right))
    } else {
      seq
    }
  }

  private def functionalMerge[T <: Comparable[T]](a: List[T], b: List[T])(implicit ord: Ordering[T]): List[T] = {
    (a, b) match {
      case (v, Nil) => v
      case (Nil, v) => v
      case (x :: xs, y :: ys) =>
        if (ord.lt(x, y)) {
          x +: functionalMerge(xs, b)
        } else {
          y +: functionalMerge(a, ys)
        }

      case _ => Nil
    }
  }


  private def inplaceSort[T <: Comparable[T]](array: Array[T], from: Int, to: Int)(implicit ord: Ordering[T]): Unit = {
    val length = to - from + 1
    if (length > 1) {
      // Same as (to + from) / 2, but avoids overflow for large from and to
      val middle = from + (to - from) / 2
      inplaceSort(array, from, middle)
      inplaceSort(array, middle + 1, to)
      inplaceMerge(array, from, to, middle)
    }
  }

  private def inplaceMerge[T <: Comparable[T]](array: Array[T], from: Int, to: Int, middle: Int)(implicit ord: Ordering[T]): Unit = {
    var left = from
    var mid = middle
    var right = middle + 1

    if (ord.gt(array(mid), array(right))) {
      while (left <= mid && right <= to) {

        if (ord.lteq(array(left), array(right))) {
          left += 1
        } else {
          val value = array(right)
          var index = right

          // Shift all the elements between element 1
          // element 2, right by 1.
          while (index != left) {
            array(index) = array(index - 1)
            index -= 1
          }
          array(left) = value

          left += 1
          mid += 1
          right += 1
        }
      }
    }
  }

}

object MergeSortApp extends App with SortTesting {

  //  testInplace(MergeSort.sort[Integer])

  testInplace(MergeSort.sortInplace[Integer])

}
