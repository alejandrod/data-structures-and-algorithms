package sections.section6

import scala.collection.mutable

object ArraysLetCode {


  //https://leetcode.com/problems/two-sum/description/
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val complements = new mutable.HashMap[Int, Int]

    val r = nums.indices.to(LazyList).map(i => {
      val current = nums(i)
      val complement = target - current
      val seenComplement = complements.get(complement)
      if (seenComplement.isDefined) {
        Some((seenComplement.get, i))
      } else {
        complements.addOne((current, i))
        None
      }
    })

    r.find(_.isDefined).flatten.map(e => Array(e._1, e._2)).getOrElse(Array.empty)
  }


  // Kadane's algorithm
  // https://www.geeksforgeeks.org/largest-sum-contiguous-subarray/
  def maxSubArray(nums: Array[Int]): Int = {
    val n = nums.length
    var maxEnding = 0
    var maxSum = Integer.MIN_VALUE

    var i = 0
    while (i < n) {
      maxEnding += nums(i)
      if (maxEnding > maxSum) {
        maxSum = maxEnding
      }
      // This is the trick. Basically it makes sure that we consider previous accumulation only if positive.
      // If positive, it adds up for the sum. Otherwise it won't.
      if (maxEnding < 0) {
        maxEnding = 0
      }

      i += 1
    }

    maxSum
  }

  // https://leetcode.com/problems/move-zeroes/description/
  def moveZeroes(nums: Array[Int]): Unit = {
    val n = nums.length
    var i, zerosCount = 0

    while (i < n) {
      val current = nums(i)
      if (current == 0) {
        zerosCount += 1
      } else if (zerosCount > 0) {
        val shiftIndex = i - zerosCount
        nums(shiftIndex) = current
        nums(i) = 0
      }
      i += 1
    }
  }


  // https://leetcode.com/problems/contains-duplicate/submissions/
  def containsDuplicate(nums: Array[Int]): Boolean = {
    val set = new mutable.HashSet[Int]()
    nums.exists(e => {
      if (set.contains(e)) {
        true
      } else {
        set.add(e)
        false
      }
    })
  }

  //https://leetcode.com/problems/rotate-array/description/
  def rotate(nums: Array[Int], k: Int): Unit = {
    if (k > 0) {
      val n = nums.length
      // Maybe using nums will be faster.
      val storage = new Array[Int](k.min(n))
      Array.copy(nums, 0, storage, 0, storage.length)
      var i = 0

      while (i < n) {
        val shiftIndex = (i + k) % n
        val storeIndex = (i + k.min(n)) % k.min(n)

        val previousVal = nums(shiftIndex)
        nums(shiftIndex) = storage(storeIndex)
        storage(storeIndex) = previousVal

        i += 1
      }
    }
  }

  //https://coderbyte.com/editor/Longest%20Word
  def longestWord(text: String): String = {
    text.split(" ").maxBy(s => s.count(_.isLetter))
  }
}


object ArraysLetCodeApp extends App {

  import ArraysLetCode._

  assert(twoSum(Array(2, 7, 11, 15), 9) sameElements Array(0, 1))

  assert(maxSubArray(Array(-2, 1, -3, 4, -1, 2, 1, -5, 4)) == 6)
  assert(maxSubArray(Array(-2, -1)) == -1)
  assert(maxSubArray(Array(-1, -2)) == -1)
  assert(maxSubArray(Array(8, -19, 5, -4, 20)) == 21)
  assert(maxSubArray(Array(1, 2)) == 3)
  assert(maxSubArray(Array(1, 2, -1, -2, 2, 1, -2, 1)) == 3)
  assert(maxSubArray(Array(-4, -2, -22, -1, -3)) == -1)


  val moveZeroesInput1 = Array(0, 1, 0, 3, 12)
  moveZeroes(moveZeroesInput1)
  assert(moveZeroesInput1 sameElements Array(1, 3, 12, 0, 0))

  val moveZeroesInput2 = Array(0, 1, 0)
  moveZeroes(moveZeroesInput2)
  assert(moveZeroesInput2 sameElements Array(1, 0, 0))

  assert(!containsDuplicate(Array(-4, -2, -22, -1, -3)))
  assert(containsDuplicate(Array(-4, -2, -22, -1, -3, -1)))


  val rotateInput1 = Array(1, 2, 3, 4, 5, 6, 7)
  rotate(rotateInput1, 3)
  assert(rotateInput1 sameElements Array(5, 6, 7, 1, 2, 3, 4))

  val rotateInput2 = Array(1, 2)
  rotate(rotateInput2, 3)
  assert(rotateInput2 sameElements Array(2, 1))

  assert(longestWord("fun&!! time") equals "time")
  assert(longestWord("I love dogs") equals "love")
}