package imperative_to_functional.counter.mutable

class Counter(private[this] var _count: Int) {
  def count = _count
  def inc(): Unit = _count += 1
  def dec(): Unit = _count -= 1
}

object CounterApp extends App {
  def makeCount(c: Counter): Vector[Int] = {
    val count0  = counter.count
    val count1  = { counter.inc(); counter.count }
    val count2  = { counter.inc(); counter.count }
    val count3  = { counter.dec(); counter.count }
    Vector(count0, count1, count2, count3)
  }

  val counter = new Counter(0)
  val counts = makeCount(counter)
  println(s"counter=$counter, counts=$counts")
}
