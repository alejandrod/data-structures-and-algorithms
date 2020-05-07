package sections.section6

object IQ68ReverseString {

  // Of course you can always do s.reverse
  def reverse(s: String): String = {
    val myArray = new MyArray[Char]
    for {
      i <- Range.inclusive(s.length -1, 0, -1)
    } {
      myArray.push(s(i))
    }

    myArray.mkString
  }

}

object IQ68ReverseStringApp extends App {
  import IQ68ReverseString._

  println(reverse("alejandro"))
  println(reverse("a"))
  println(reverse(""))
}
