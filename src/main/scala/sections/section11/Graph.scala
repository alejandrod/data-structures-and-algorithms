package sections.section11

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Graph[T] {

  private var numberOfNodes = 0
  private val adjacentList = new mutable.HashMap[T, ListBuffer[T]]

  def addVertex(node: T): ListBuffer[T] = {
    adjacentList.getOrElse(node, {
      val edges = ListBuffer[T]()
      adjacentList(node) = edges
      numberOfNodes += 1
      edges
    })
  }

  def addEdge(node1: T, node2: T): Graph[T] = {
    addVertex(node1).addOne(node2)
    addVertex(node2).addOne(node1)
    this
  }

  def showConnections: String = {
    val lines = adjacentList.map {
      case (node, edges: ListBuffer[T]) => s"$node --> ${edges.mkString(" ")}"
    }
    lines.mkString("\n")
  }
}

object GraphApp extends App {
  val myGraph = new Graph[Int]

  myGraph.addVertex(0)
  myGraph.addVertex(1)
  myGraph.addVertex(2)
  myGraph.addVertex(3)
  myGraph.addVertex(4)
  myGraph.addVertex(5)
  myGraph.addVertex(6)
  myGraph.addEdge(3, 1)
  myGraph.addEdge(3, 4)
  myGraph.addEdge(4, 2)
  myGraph.addEdge(4, 5)
  myGraph.addEdge(1, 2)
  myGraph.addEdge(1, 0)
  myGraph.addEdge(0, 2)
  myGraph.addEdge(6, 5)

  println(myGraph.showConnections)
}
