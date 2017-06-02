package tutorial.arun

/**
  * @author arunvarma.
  */
object SqrtExample {

  def main(args: Array[String]): Unit = {

    val x = 0.3456

    println("Square root of " + x + ": " + sqrt(x))

    //println("Square root of " + x + ": " + sqrt2(x))
  }

  /**
    * square root method using newton's method of successive approximations.
    *
    * isGoodEnough method goes in to infinite for very big number (1E13) ?? why??
    * @param x value to calculate square root
    * @return square root of supplied value
    */
  def sqrt(x: Double): Double = {
    def sqrtIter(guess: Double, x: Double): Double = {
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)
    }

    def isGoodEnough(guess: Double, x: Double): Boolean = Math.abs(Math.pow(guess, 2) - x) < 0.001

    def improve(guess: Double, x: Double): Double = (guess + x / guess) / 2

    sqrtIter(1.0, x)
  }

  /**
    * optimized square root method.
    * isGoodEnough is optimized to calculate very big or very small number.
    *
    * @param x value to calculate square root
    * @return square root of supplied value
    */
  def sqrt2(x: Double): Double = {

    def sqrtIter(guess: Double, oldGuess: Double, x: Double): Double = {
      if (isGoodEnough(guess, oldGuess)) {
        println(guess + "\t" + oldGuess + "\t" + x)
        guess
      }
      else {
        println(guess + "\t" + oldGuess + "\t" + x)
        sqrtIter(improve(guess, x), guess, x)
      }
    }

    def isGoodEnough(guess: Double, oldGuess: Double): Boolean = Math.abs(guess - oldGuess) < (0.001 * guess)

    def improve(guess: Double, x: Double): Double = (guess + x / guess) / 2

    println("guess\toldGuess\tx")
    sqrtIter(1.0, 0.0, x)
  }

}
