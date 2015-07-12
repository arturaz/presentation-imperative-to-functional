package imperative_to_functional.fizz_buzz.imperative

import scala.io.StdIn

object FizzBuzz extends App { /* Imperative */
  var line = ""
  def read(): Unit = {
    print("Enter a number (done to finish): ")
    line = StdIn.readLine().stripLineEnd
  }

  read()
  while (line != "done") {
    try {
      val int = line.toInt
      if (int % 3 == 0 && int % 5 == 0) println("fizzbuzz")
      else if (int % 3 == 0) println("fizz")
      else if (int % 5 == 0) println("buzz")
    }
    catch {
      case _: NumberFormatException => println(s"$line was not a number!")
    }

    read()
  }
}
