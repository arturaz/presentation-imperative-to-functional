package imperative_to_functional.counter.scalaz

import scalaz._, Scalaz._

case class Counter(count: Int)

object Counter {
  def doAndGet(f: Counter => Counter): State[Counter, Int] =
    State { (c: Counter) =>
      val nc = f(c)
      (nc, nc.count)
    }
  val count = State.gets((_: Counter).count)
  val inc = doAndGet(c => c.copy(count = c.count + 1))
  val dec = doAndGet(c => c.copy(count = c.count - 1))
}

object CounterApp extends App {
  val incTwice: State[Counter, Vector[Int]] =
    // .sequenceU is Vector[State[Counter, Int]] => State[Counter, Vector[Int]]
    // or more generically M[G[A]] => G[M[A]]
    Vector(Counter.count, Counter.inc, Counter.inc, Counter.dec).sequenceU
  val (currentCounter, counts) = incTwice(Counter(0))
  println(s"counter=$currentCounter, counts=$counts")
}