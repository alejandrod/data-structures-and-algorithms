package sections.section10

import scala.annotation.tailrec

case class BinaryTreeNode[T](var value: T, var left: Option[BinaryTreeNode[T]] = None, var right: Option[BinaryTreeNode[T]] = None) {

  override def toString: String = value.toString

  def children: List[BinaryTreeNode[T]] = left.toList ::: right.toList
}

class BinarySearchTree[T <: Comparable[T]] {
  type Node = BinaryTreeNode[T]

  private var _root: Option[Node] = None

  def root: Option[Node] = _root

  def insert(e: T): Node = {
    if (_root.isEmpty) {
      _root = Some(BinaryTreeNode(e))
      _root.get
    } else {
      doInsert(e, _root.get)
    }
  }

  def lookup(e: T): Option[Node] = {
    if (_root.isEmpty) {
      None
    } else {
      doLookup(e, _root.get)
    }
  }

  def remove(e: T): Option[Node] = {
    if (_root.isEmpty) {
      None
    } else {
      doRemove(e, _root.get, None)
    }
  }


  override def toString: String = {
    val levels = LazyList.range(1, Int.MaxValue).map(level => {
      nodesForLevel(_root, level)
    }).takeWhile(_.nonEmpty)

    levels.map(nodes => nodes.mkString(" - ")).mkString("\n")
  }

  //TODO: make tail recursive
  private def nodesForLevel(node: Option[Node], level: Int): List[Node] = {
    if (node.isEmpty) {
      Nil
    } else if (level == 1) {
      List(node.get)
    } else {
      nodesForLevel(node.get.left, level - 1) ::: nodesForLevel(node.get.right, level - 1)
    }
  }

  @tailrec
  private def doInsert(e: T, current: Node): Node = {
    if (e.compareTo(current.value) >= 0) {
      if (current.right.isEmpty) {
        val node = BinaryTreeNode(e)
        current.right = Some(node)
        node
      } else {
        doInsert(e, current.right.get)
      }
    } else {
      if (current.left.isEmpty) {
        val node = BinaryTreeNode(e)
        current.left = Some(node)
        node
      } else {
        doInsert(e, current.left.get)
      }
    }
  }

  @tailrec
  private def doLookup(e: T, current: Node): Option[Node] = {
    val comparision = e.compareTo(current.value)
    if (comparision > 0) {
      if (current.right.isDefined) {
        doLookup(e, current.right.get)
      } else {
        None
      }
    } else if (comparision < 0) {
      if (current.left.isDefined) {
        doLookup(e, current.left.get)
      } else {
        None
      }
    } else {
      Some(current)
    }
  }

  @tailrec
  private def doRemove(e: T, current: Node, parent: Option[Node]): Option[Node] = {
    val comparison = e.compareTo(current.value)
    if (comparison > 0) {
      if (current.right.isDefined) {
        doRemove(e, current.right.get, Some(current))
      } else {
        None
      }
    } else if (comparison < 0) {
      if (current.left.isDefined) {
        doRemove(e, current.left.get, Some(current))
      } else {
        None
      }
    } else {
      //We have a match, get to work!

      //Option 1: No right child:
      if (current.right.isEmpty) {
        if (parent.isEmpty) {
          _root = current.left
        } else {
          val c = current.value.compareTo(parent.get.value)
          if (c < 0) {
            parent.get.left = current.left
          } else if (c > 0) {
            parent.get.right = current.left
          }
        }

        //Option 2: Right child which doesnt have a left child
      } else if (current.right.get.left.isEmpty) {
        current.right.get.left = current.left

        if (parent.isEmpty) {
          _root = current.right
        } else {
          val c = current.value.compareTo(parent.get.value)
          if (c < 0) {
            parent.get.left = current.right
          } else if (c > 0) {
            parent.get.right = current.right
          }
        }

        //Option 3: Right child that has a left child
      } else {

        //find the Right child's left most child
        var leftmost = current.right.get.left.get
        var leftmostParent = current.right.get
        while (leftmost.left.isDefined) {
          leftmostParent = leftmost
          leftmost = leftmost.left.get
        }

        //Parent's left subtree is now leftmost's right subtree
        leftmostParent.left = leftmost.right
        leftmost.left = current.left
        leftmost.right = current.right

        if (parent.isEmpty) {
          this._root = Some(leftmost)
        } else {
          val c = current.value.compareTo(parent.get.value)
          if (c < 0) {
            parent.get.left = Some(leftmost)
          } else if (c > 0) {
            parent.get.right = Some(leftmost)
          }
        }
      }

      Some(current)
    }
  }

}

object BinarySearchTreeApp extends App {

  val tree = new BinarySearchTree[Integer]

  tree.insert(9)
  tree.insert(4)
  tree.insert(6)
  tree.insert(20)
  tree.insert(170)
  tree.insert(15)
  tree.insert(1)

  println(tree)

  println(tree.lookup(9))
  println(tree.lookup(6))
  println(tree.lookup(170))
  println(tree.lookup(33))

  println(tree.remove(4))
  println(tree)
}
