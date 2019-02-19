import scala.util.Try

object App1 {
  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  //describes a single interaction with the external world that produces an A
  //immutable data structure
  //no ability to see inside here
  //unsafeRun - embedding a hunk of code - that could contain anything that we cannot easily look inside
  case class IO[A](unsafeRun: () => A) { self =>
    def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun()))

    def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(self.unsafeRun()).unsafeRun())
  }

  object IO {
    def point[A](a: A): IO[A] = new IO(() => a)
  }

  def putStrLn(line: String): IO[Unit] = IO(() => println(line))

  def getStrLn: IO[String] = IO(() => readLine())

  def nextInt(upper: Int): IO[Int] = IO(() => new scala.util.Random().nextInt(upper))

  def checkContinue(name: String): IO[Boolean] = {
    for {
      _ <- putStrLn("Do you want to continue, " + name + "?")
      line <- getStrLn.map(_.toLowerCase)
      cont <- line match {
        case "y" => IO.point(true)
        case "n" => IO.point(false)
        case _ => IO.point(true)
      }
    } yield cont

  }

  //not tail recursive - scala's for comprehension add a final map so can't finish in tail-call position
  def gameLoop(name: String): IO[Unit] = {
    for {
      num <- nextInt(5).map(_ + 1)
      _ <- putStrLn("Dear " + name + ", please guess a number from 1 to 5:")
      input <- getStrLn
      //fold over an option to produce an IO[Unit]
      _ <- parseInt(input).fold[IO[Unit]](
        putStrLn("You did not enter a number")
      )(guess =>
        if (guess == num) putStrLn("You guessed right " + name + "!")
        else putStrLn("You guessed wrong, " + name + "! The number was: " + num))
      cont <- checkContinue(name)
      _  <- if (cont) gameLoop(name) else IO.point(())
    } yield ()
  }

  def main: IO[Unit] = {
    /**
      * Step 1. eliminate sources of partiality, lift partial function out into a total function
      * Use data structures like Either, Option
      * No runtime errors
      * Represent errors with data structures
      *
      *
      * If you run this currently in the terminal, nothing will happen.
      * It just builds up this IO that's a value composed from all these other things (map, flatMap, point)
      * and then at the top level of program you call unsafe run and that's when things start to happen
      **/

    for {
      _ <- putStrLn("What is your name?")
      name <- getStrLn
      _ <- putStrLn("Hello, " + name + ", welcome to the game!")
      _ <- gameLoop(name)
    } yield ()
  }
}
