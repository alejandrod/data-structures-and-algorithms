package sections.section14

import sections.section10.BinarySearchTree

// recursion is ok as it should be log(n)
object DepthFirstSearch {

  def traverseInOrder[T <: Comparable[T]](graph: BinarySearchTree[T]): List[T] = {

    def loop(current: Option[graph.Node]): List[T] = {
      current match {
        case Some(node) => loop(node.left) ::: List(node.value) ::: loop(node.right)
        case None => Nil
      }
    }

    loop(graph.root)
  }

  def traversePreOrder[T <: Comparable[T]](graph: BinarySearchTree[T]): List[T] = {

    def loop(current: Option[graph.Node]): List[T] = {
      current match {
        case Some(node) => node.value :: loop(node.left) ::: loop(node.right)
        case None => Nil
      }
    }

    loop(graph.root)
  }

  def traversePostOrder[T <: Comparable[T]](graph: BinarySearchTree[T]): List[T] = {

    def loop(current: Option[graph.Node]): List[T] = {
      current match {
        case Some(node) => loop(node.left) ::: loop(node.right) ::: List(node.value)
        case None => Nil
      }
    }

    loop(graph.root)
  }
}

object DepthFirstSearchApp extends App {

  import DepthFirstSearch._

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

  assert(traverseInOrder(tree) sameElements List(1, 4, 6, 9, 15, 20, 170))
  assert(traversePreOrder(tree) sameElements List(9, 4, 1, 6, 20, 15, 170))
  assert(traversePostOrder(tree) sameElements List(1, 6, 4, 15, 170, 20, 9))

}
