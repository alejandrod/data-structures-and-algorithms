package sections.section15

// https://leetcode.com/problems/climbing-stairs/
object ClimbingStairs {
  def climbStairs(n: Int): Int = {
    if (n > 0) {
      val cache = new scala.collection.mutable.HashMap[Int, Int]()

      def permutationsFrom(step: Int): Int = {
        cache.getOrElseUpdate(step, {
          if (step >= n) {
            0
          } else if (step == n - 1) {
            1
          } else if (step == n - 2) {
            2
          } else {
            permutationsFrom(step + 1) + permutationsFrom(step + 2)
          }
        })
      }

      permutationsFrom(0)
    } else {
      0
    }
  }
}

object ClimbingStairsApp extends App {

  import ClimbingStairs._

  assert(climbStairs(0) == 0)
  assert(climbStairs(2) == 2)
  assert(climbStairs(3) == 3)
}
