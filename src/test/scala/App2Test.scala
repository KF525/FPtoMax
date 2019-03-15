import App2.{Console, IO, Program, Random, gameLoop, getStrLn, putStrLn}
import org.scalatest.FlatSpec

import scala.Console.println

class App2Test extends FlatSpec {
  //test without any interaction with the external world

  //fake input and collects output, and fake random numbers and has implementation of these type classes
  //now has capability of composing Programs sequentially
  case class TestData(input: List[String], output: List[String], nums: List[Int]) {
    def putStrLn(line: String): (TestData, Unit) =
      (copy(output = line :: output), ())

    def getStrLn: (TestData, String) =
      (copy(input = input.drop(1)), input.head)

    def nextInt(upper: Int): (TestData, Int) =
      (copy(nums = nums.drop(1)), nums.head)

    //render this in some way
    def showResults = output.reverse.mkString("\n")
  }

  //like IO but not IO - represents a program that produces an A
  //if you give us testdata we will return more test data and the A
  case class TestIO[A](run: TestData => (TestData, A)) { self =>
    def map[B](ab: A => B): TestIO[B] = TestIO(t => self.run(t) match {case (t, a) => (t, ab(a))})
    //threading test data - threading state through our computations (State Monad)
    def flatMap[B](afb: A => TestIO[B]): TestIO[B] =
      TestIO(t => self.run(t) match { case (t, a) => afb(a).run(t)})

    def eval(t: TestData) = run(t)._1
  }

  object TestIO {
    def point[A](a: A): TestIO[A] = TestIO(t => (t, a))

    implicit val ProgramTestIO = new Program[TestIO] {
      def finish[A](a: => A): TestIO[A] = TestIO.point(a)
      def chain[A, B](fa: TestIO[A], afb: A => TestIO[B]): TestIO[B] = fa.flatMap(afb)
      def map[A, B](fa: TestIO[A], ab: A => B): TestIO[B] = fa.map(ab)
    }

    implicit val ConsoleTestIO = new Console[TestIO] {
      def putStrLn(line: String): TestIO[Unit] = TestIO(t => t.putStrLn(line))
      def getStrLn: TestIO[String] = TestIO(t => t.getStrLn)
    }

    implicit val RandomTestIO = new Random[TestIO] {
      def nextInt(upper: Int): TestIO[Int] = TestIO(t => t.nextInt(upper))
    }
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

  def mainTestIO: TestIO[Unit] = main[TestIO]

  val testExample =
    TestData(
      input = "John" :: "1" :: "n" :: Nil,
      output = Nil, //output start as empty
      nums = 0 :: Nil)

  def runTest = mainTestIO.eval(testExample).showResults //showResults to get back a string
}
