package imperative_to_functional.fizz_buzz.scalaz

import scalaz._, Scalaz._
import scalaz.effect._

object FizzBuzz extends SafeApp { /* Scalaz */
  def int2answer(i: Int) =
    if (i % 3 == 0 && i % 5 == 0) Some("fizzbuzz")
    else if (i % 3 == 0) Some("fizz")
    else if (i % 5 == 0) Some("buzz")
    else None

  val read: IO[String] = for {
    _ <- IO.putStr("Enter a number (done to finish): ")
    line <- IO.readLn.map(_.stripLineEnd)
  } yield line

  def line2print(line: String): IO[Unit] = parseInt(line).fold(
    _ => IO.putStrLn(s"$line was not a number!"),
    int2answer(_).fold(IO.ioUnit)(IO.putStrLn)
  )

  override def runc: IO[Unit] = {
    lazy val main: IO[Unit] = for {
      line <- read
      _ <- if (line == "done") IO.ioUnit
           else line2print(line) flatMap (_ => main)
    } yield ()

    main
  }
}
