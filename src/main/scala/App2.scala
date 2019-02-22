import scala.io.StdIn.readLine
import scala.util.Try
import scala.Console._
/**
  * Make it polymorphic - Doesn't have to be IO - more generic
  * Give up reliance on concrete data type. Develop ability to abstract over things that look like IO but are not IO.
  * Helps us test better without interacting with the real world.
  * Introduce a type class, Program.
  *
  * Different aspects of our program have different requirements (Random, Console, Program).
  */
object App2 {
  def parseInt(string: String): Option[Int] = Try(string.toInt).toOption

  // Program is a type constructor that takes one type parameter (IO for example)
  // monadic type
  trait Program[F[_]] {
    // represents a program that is done, that already has value
    def finish[A](a: => A): F[A]

    // chain many programs together, given one program that produces an Program[A] and another program that takes an A and returns a Program[B]
    // chaining output of one program, as input for the next program
    // sequential composition
    def chain[A, B](fa: F[A], afb: A => F[B]): F[B]

    def map[A, B](fa: F[A], ab: A => B): F[B]
  }

  object Program {
    // give us one of these things for an implicit that is in scope
    def apply[F[_]](implicit F: Program[F]): Program[F] = F
  }

  // create syntax with implicit class
  // given an F[A] add some methods (extension methods) map and flatMap => so any F[A] value will be enriched with map and flatMap methods.
  // so now if we have an instance of this type class in scope
  implicit class ProgramSyntax[F[_], A](fa: F[A]) {
    def map[B](f: A => B)(implicit F: Program[F]): F[B] = F.map(fa, f)
    def flatMap[B](afb: A => F[B])(implicit F: Program[F]): F[B] = F.chain(fa, afb)
  }

  // IO companion object
  object IO {

    def point[A](a: A): IO[A] = IO(() => a)

    // type class for Program[IO] - implement the Program methods, F is replaced with IO
    implicit val ProgramIO = new Program[IO] {
      def finish[A](a: => A): IO[A] = IO.point(a)
      def chain[A, B](fa: IO[A], afb: A => IO[B]): IO[B] = fa.flatMap(afb)
      def map[A, B](fa: IO[A], ab: A => B): IO[B] = fa.map(ab)
    }

    implicit val ConsoleIO = new Console[IO] {
      def putStrLn(line: String): IO[Unit] = IO(() => println(line))
      def getStrLn: IO[String] = IO(() => readLine())
    }

    implicit val RandomIO = new Random[IO] {
      def nextInt(upper: Int): IO[Int] = IO(() => new scala.util.Random().nextInt(upper))
    }
  }

  def finish[F[_], A](a: => A)(implicit F: Program[F]): F[A] = F.finish(a)
  trait Console[F[_]] {
    def putStrLn(line: String): F[Unit]
    def getStrLn: F[String]
  }
  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F
  }
  def putStrLn[F[_]: Console](line: String): F[Unit] = Console[F].putStrLn(line)
  def getStrLn[F[_]: Console]: F[String] = Console[F].getStrLn
  trait Random[F[_]] {
    def nextInt(upper: Int): F[Int]
  }
  object Random {
    def apply[F[_]](implicit F: Random[F]): Random[F] = F
  }
  def nextInt[F[_]](upper: Int)(implicit F: Random[F]): F[Int] = Random[F].nextInt(upper)
  //describes a single interaction with the external world that produces an A
  //immutable data structure
  //no ability to see inside here
  case class IO[A](unsafeRun: () => A) { self =>
    def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun()))
    def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(self.unsafeRun()).unsafeRun())
  }

  def checkContinue[F[_]: Program: Console](name: String): F[Boolean] = {
    for {
      _ <- putStrLn("Do you want to continue, " + name + "?")
      line <- getStrLn.map(_.toLowerCase)
      cont <- line match {
        case "y" => finish(true)
        case "n" => finish(false)
        case _ => finish(true)
      }
    } yield cont
  }
  def printResults[F[_]: Console](input: String, num: Int, name: String): F[Unit] = {
    val optionInt: Option[Int] = parseInt(input)
    optionInt match {
      case Some(i: Int) if i == num => putStrLn("You guessed right " + name + "!")
      case Some(i: Int) => putStrLn("You guessed wrong, " + name + "! The number was: " + num)
      case None => putStrLn("You did not enter a number")
    }
  }

  //not tail recursive - scala's for comprehension add a final map so can't finish in tail-call position
  def gameLoop[F[_]: Console: Program: Random](name: String): F[Unit] = {
    for {
      num <- nextInt(5).map(_ + 1)
      _ <- putStrLn("Dear " + name + ", please guess a number from 1 to 5:")
      input <- getStrLn
      _ <- printResults(input, num, name)
      cont <- checkContinue(name)
      _  <- if (cont) gameLoop(name) else finish(())
    } yield ()
  }

  def main[F[_]: Program: Random: Console]: F[Unit] = {
    for {
      _ <- putStrLn("What is your name?")
      name <- getStrLn
      _ <- putStrLn("Hello, " + name + ", welcome to the game!")
      _ <- gameLoop(name)
    } yield ()
  }
  def mainIO: IO[Unit] = main[IO]
}