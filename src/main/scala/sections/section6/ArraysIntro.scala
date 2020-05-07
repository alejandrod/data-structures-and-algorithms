package sections.section6

// https://docs.scala-lang.org/overviews/collections/performance-characteristics.html

// Variable array is somewhere in memory and the computer knows it.
// When I do array[2], i"m telling the computer, hey go to the array and grab the 3rd item from where the array is stored.

object ArraysIntro extends App {
  //NOTE: scala has Array and List. Probably, List are closer to the JS array. Let's see both
  val array = Array("a", "b", "c", "d")

  // append to the end. Different than the push operation in js
  // The methods use Array copy
  println(array :+ "e") //O(n)
  // prepend
  println("x" +: array) //O(n)

  //splice
  val (front, back) = array.splitAt(2)
  val spliced: Array[String] = (front :+ "alien") ++ back //O(n)
  println(spliced)


  val list = List("a", "b", "c", "d")

  // append to the end. Different than the push operation in js
  println(list :+ "e") //O(n)
  // prepend
  println("x" :: list) //O(1)

  //splice
  println(list.take(2) ::: List("alien") ::: list.takeRight(list.length - 2)) //O(n)
}
