package sections.section14

//https://leetcode.com/problems/validate-binary-search-tree/
object ValidBinarySearchTree {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right


    override def toString = s"$value"
  }

  def isValidBST_BFS(root: TreeNode): Boolean = {
    if (root == null) {
      true
    } else {
      val queue = scala.collection.mutable.Queue[(Option[Int], Option[Int], TreeNode)]((None, None, root))
      var valid = true

      while (valid && queue.nonEmpty) {
        val (min, max, current) = queue.dequeue()

        if (max.exists(current.value >= _) || min.exists(current.value <= _)) {
          valid = false
        } else {
          if (current.left != null) {
            queue.enqueue((min, Some(current.value), current.left))
          }
          if (current.right != null) {
            queue.enqueue((Some(current.value), max, current.right))
          }
        }
      }

      valid
    }
  }

  def isValidBST_DFS(root: TreeNode): Boolean = {
    def loop(current: TreeNode, min: Option[Int], max: Option[Int]): Boolean = {
      if (current != null) {
        if (max.exists(current.value >= _) || min.exists(current.value <= _)) {
          false
        } else {
          loop(current.left, min, Some(current.value)) && loop(current.right, Some(current.value), max)
        }
      } else {
        true
      }
    }

    loop(root, None, None)
  }

}

object ValidBstApp extends App {

  import ValidBinarySearchTree._

  def testBST(isValidBST: TreeNode => Boolean): Unit = {
    val treeSingle = new TreeNode(2147483647)
    assert(isValidBST(treeSingle))

    val tree123 = new TreeNode(2, new TreeNode(1), new TreeNode(3))
    assert(isValidBST(tree123))

    val tree213 = new TreeNode(1, new TreeNode(2), new TreeNode(3))
    assert(!isValidBST(tree213))

    val tree11 = new TreeNode(1, new TreeNode(1))
    assert(!isValidBST(tree11))

    val tree5_1 = new TreeNode(5, new TreeNode(1), new TreeNode(4, new TreeNode(3), new TreeNode(6)))
    assert(!isValidBST(tree5_1))

    //     10
    //  5     15
    //      6    20
    val tree15_5 = new TreeNode(10, new TreeNode(5), new TreeNode(15, new TreeNode(6), new TreeNode(20)))
    assert(!isValidBST(tree15_5))

    //       3
    //  1        5
    //0   2   4    6
    val tree3_1 = new TreeNode(3,
      new TreeNode(1, new TreeNode(0), new TreeNode(2)),
      new TreeNode(5, new TreeNode(4), new TreeNode(6)))
    assert(isValidBST(tree3_1))

    // [3,1,5,0,2,4,6,null,null,null,3] = false
    //           3
    //      1        5
    //    0   2   4    6
    //          3
    val tree3_1_3 = new TreeNode(3,
      new TreeNode(1, new TreeNode(0), new TreeNode(2, null, new TreeNode(3))),
      new TreeNode(5, new TreeNode(4), new TreeNode(6)))
    assert(!isValidBST(tree3_1_3))
  }

  testBST(isValidBST_BFS)
  testBST(isValidBST_DFS)
}
