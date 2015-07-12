package imperative_to_functional.counter.immutable

case class Counter(count: Int) {
  def inc = copy(count = count + 1)
  def dec = copy(count = count - 1)
}

object CounterApp extends App {
  def makeCount(c: Counter): (Counter, Vector[Int]) = {
    val c1 = c.inc
    val c2 = c1.inc
    val c3 = c2.dec
    (c3, Vector(c.count, c1.count, c2.count, c3.count))
  }

  val (currentCounter, counts) = makeCount(Counter(0))
  println(s"counter=$currentCounter, counts=$counts")
}