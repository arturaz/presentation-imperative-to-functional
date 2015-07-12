package imperative_to_functional.fizz_buzz.immutable

import scala.io.StdIn

object FizzBuzz extends App { /* Functional */
  class Later[A](private val f: () => A) extends AnyVal {
    /* Returns a new Later which will combine this and l2 when evaluated. */
    def flatMap[B](l2: A => Later[B]): Later[B] =
      Later { l2(f()).endOfTheUniverse_!() }

    /* Just evaluates f. Call at the end of the program. */
    def endOfTheUniverse_!(): A = f()
  }
  object Later {
    def apply[A](f: => A): Later[A] = new Later(() => f)
  }

  def int2answer(i: Int): Option[String] =
    if (i % 3 == 0 && i % 5 == 0) Some("fizzbuzz")
    else if (i % 3 == 0) Some("fizz")
    else if (i % 5 == 0) Some("buzz")
    else None

  val read: Later[String] = Later {
    print("Enter a number (done to finish): ")
    StdIn.readLine().stripLineEnd
  }

  def line2print(line: String): Later[Unit] = Later {
    try int2answer(line.toInt).foreach(println)
    catch {
      case _: NumberFormatException => println(s"$line was not a number!")
    }
  }

  lazy val main: Later[Unit] = read.flatMap {
    case "done" => Later(())
    case line => line2print(line).flatMap( _ => main)
  }

  main.endOfTheUniverse_!()
}
