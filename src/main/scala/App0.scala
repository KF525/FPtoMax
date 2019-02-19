class App0 {
  def main: Unit = {
    println("What is your name?")

    val name = readLine()

    println("Hello, " + name + ", welcome to the game!" )

    var exec = true

    while (exec) {
      val num = new scala.util.Random().nextInt(5) + 1

      println("Dear " + name + ", please guess a number from 1 to 5:")

      // Partial function - lacks totality
      val guess = scala.io.StdIn.readInt()

      if (guess == num) println("You guessed right " + name + "!")
      else println("You guessed wrong, " + name + "! The number was: " + num)

      println("Do you want to continue, " + name + "?")

      //if the use does not input correctly y or n it will crash - lacks totality - only handle two Strings
      readLine() match {
        case "y" => exec = true
        case "n" => exec = false
      }
    }
  }
}

/*
There are a bunch of partial functions in this program,
results in partial exceptions adn then you'll end up blowing stuff up
Step 1: Eliminate partial functions
*/

object Intro {
  /**
    * Functional programming is all about programming with functions.
    * Functions are:
    *
    * 1. Total. For every input they return an output
    * 2. Deterministic. For the same input, they return the same output.
    * 3. Pure. Their only effect is computing the return value.
    *
    * These properties help us
    *
    * 1. Reason about our programs using equational reason
    * 2. Refactor our programs without changing their meaning
    * 3. Test our programs more easily
    * 4. Invert control so caller always has control over callee
    * 5. Reason about our programs using type-based reasoning
    *
    * Build project from mathematical functions and immutable data structures
    * (versus procedural functions - list of instructions that you send to your CPU
    * telling it to update this memory location and send something over the wire and so on
    *
    * caller always has control over what the callee does,
    * the callee can only return a value and the caller can decide
    * what to do with that value
    *
    * pushing power up and deferring things higher and higher leads to
    * programs that are easier to understand
    */

  //Not mathematical functions...
  def println(s: String): Unit = ???   //impure
  def readLine(): String = ???   //not deterministic - in FP it would be constant String
  def parseInt(s: String): Int = ??? //not total

}
