package sections.section15

// https://leetcode.com/problems/best-time-to-buy-and-sell-stock/
object BestTimeToBuyAndSellStock {
  def maxProfit(prices: Array[Int]): Int = {
    if (prices != null && prices.length > 1) {
      val cache = new scala.collection.mutable.HashMap[Int, Int]()

      def bestSellPrice(i: Int): Int = {
        cache.getOrElseUpdate(i, {
          if (i < prices.length) {
            val future = bestSellPrice(i + 1)
            prices(i).max(future)
          } else {
            0
          }
        })
      }

      prices.indices.map(i => bestSellPrice(i) - prices(i)).filter(_ >= 0).max
    } else {
      0
    }
  }
}

object BestTimeToBuyAndSellStockApp extends App {

  import BestTimeToBuyAndSellStock._

  assert(maxProfit(Array()) == 0)
  assert(maxProfit(Array(1)) == 0)
  assert(maxProfit(Array(7, 1, 5, 3, 6, 4)) == 5)
  assert(maxProfit(Array(7, 6, 4, 3, 1)) == 0)
}
