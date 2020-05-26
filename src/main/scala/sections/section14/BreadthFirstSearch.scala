package sections.section14

import sections.section10.{BinarySearchTree, BinaryTreeNode}

import scala.collection.{immutable, mutable}

object BreadthFirstSearch {

  def traverse[T](root: BinaryTreeNode[T]): List[BinaryTreeNode[T]] = {
    val list = mutable.ListBuffer[BinaryTreeNode[T]]()
    val queue = mutable.Queue[BinaryTreeNode[T]]()
    queue.enqueue(root)

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      list += current
      current.left.foreach(queue.enqueue)
      current.right.foreach(queue.enqueue)
    }

    list.toList
  }

  // Looks more complicated... Sometimes iterate is simpler
  def traverseFunctional[T](node: BinaryTreeNode[T]): LazyList[BinaryTreeNode[T]] = {
    def recurse(queue: immutable.Queue[BinaryTreeNode[T]]): LazyList[BinaryTreeNode[T]] = {
      if (queue.isEmpty) {
        LazyList.empty
      } else {
        val (node, tail) = queue.dequeue
        node #:: recurse(tail ++ node.children)
      }
    }

    node #:: recurse(immutable.Queue.empty ++ node.children)
  }
}

object BreadthFirstSearchApp extends App {

  import BreadthFirstSearch._

  val tree = new BinarySearchTree[Integer]()
  tree.insert(9)
  tree.insert(4)
  tree.insert(6)
  tree.insert(20)
  tree.insert(170)
  tree.insert(15)
  tree.insert(1)

  //     9
  //  4     20
  //1  6  15  170

  assert(traverse(tree.root.get).map(_.value) sameElements List(9, 4, 20, 1, 6, 15, 170))
  assert(traverseFunctional(tree.root.get).map(_.value) sameElements List(9, 4, 20, 1, 6, 15, 170))

}
