package sections.section15

// https://leetcode.com/problems/house-robber/
object HouseRobber {

  def rob(nums: Array[Int]): Int = {
    if (nums != null && nums.length > 0) {
      val cache = new scala.collection.mutable.HashMap[Int, Int]()

      def calRob(i: Int): Int = {
        cache.getOrElseUpdate(i, {
          if (i < nums.length) {
            val o1 = nums(i) + calRob(i + 2)
            val o2 = nums(i) + calRob(i + 3)
            o1.max(o2)
          } else {
            0
          }
        })
      }

      nums.indices.map(calRob).max
    } else {
      0
    }
  }

}

object HouseRobberApp extends App {

  import HouseRobber._

  assert(rob(Array()) == 0)
  assert(rob(Array(1)) == 1)
  assert(rob(Array(1, 2)) == 2)
  assert(rob(Array(1, 2, 10, 1)) == 11)
  assert(rob(Array(1, 2, 3, 1)) == 4)
  assert(rob(Array(2, 7, 9, 3, 1)) == 12)
  assert(rob(Array(2, 1, 1, 2)) == 4)

  assert(rob(Array(226, 174, 214, 16, 218, 48, 153, 131, 128, 17, 157, 142, 88, 43, 37, 157, 43, 221, 191,
    68, 206, 23, 225, 82, 54, 118, 111, 46, 80, 49, 245, 63, 25, 194, 72, 80, 143, 55, 209, 18, 55, 122, 65,
    66, 177, 101, 63, 201, 172, 130, 103, 225, 142, 46, 86, 185, 62, 138, 212, 192, 125, 77, 223, 188, 99, 228,
    90, 25, 193, 211, 84, 239, 119, 234, 85, 83, 123, 120, 131, 203, 219, 10, 82, 35, 120, 180, 249, 106, 37, 169,
    225, 54, 103, 55, 166, 124)) == 7102)

}
