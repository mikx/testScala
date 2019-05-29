package example

import scala.util.Random

object Lowes extends App {

  val prefix = 47160
  val random =  Random.nextInt() % 100000
  val offer  = 2720

  val code = prefix * 1000000000L + random * 10000 + offer

  val check = generateCheck(code)

  println(code.toString + check)

  def generateCheck(code: Long): Int = {
    val odd = sumDigits(code, true)
    val even = sumDigits(code, false)
    (10 - (odd * 3 + even) % 10) % 10
  }

  def sumDigits(n: Long, b: Boolean): Int = if (n == 0) 0 else (if (b) (n % 10).toInt else 0) + sumDigits(n / 10, !b)

}
