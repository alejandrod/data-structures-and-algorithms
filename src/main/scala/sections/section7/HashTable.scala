package sections.section7

import scala.collection.mutable.ListBuffer

class HashTable[V](size: Int = 100) {

  private val data: Array[ListBuffer[(String, V)]] = Array.fill(size)(new ListBuffer())

  private def hash(key: String): Int = {
    key.zipWithIndex.foldLeft(0) { (i, e) =>
      val (c, h) = e
      (h + c.toInt * i) % size
    }
  }

  def get(key: String): Option[V] = {
    val h = hash(key)
    data(h).find(_._1 == key).map(_._2)
  }

  def set(key: String, value: V): Unit = {
    val address = hash(key)
    data(address).addOne((key, value))
  }

  def keys: Iterator[String] = {
    data.iterator.flatMap(_.map(_._1).iterator)
  }

  override def toString: String = data.toList.toString()
}


object HashTableApp extends App {

  val myHashTable = new HashTable[Int](50)
  myHashTable.set("grapes", 10000)

  println(myHashTable)
  println(myHashTable.get("grapes"))
  println(myHashTable.get("lala"))

}