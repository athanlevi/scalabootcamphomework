package net.savinko
package _1_basics

object Basics extends App {
  // task 1 - lcm

  /**
   * The definition: https://www.calculatorsoup.com/calculators/math/lcm.php
   * Also refers to Lowest Common Multiple (LCM) and Least Common Divisor (LCD)
   *
   * Least Common Multiple
   *
   * @param a first arg
   * @param b second arg
   * @return least common multiple of two agrs
   */
  def lcmSimple(a: Int, b: Int): Int = (a % b, b % a) match {
    case (0, _) => a
    case (_, 0) => b
    case _ => a * b
  }

  /**
   * Improved version of lcm function which can consume 1..n arguments.
   *
   * @param arg  at least one argument requires
   * @param args 0..n rest of the arguments
   * @return least common multiple of two agrs
   */
  def lcm(arg: Int, args: Int*): Int = {
    def isLcmForRest(n: Int, rest: List[Int]): Boolean = rest.forall(current => n % current == 0)

    if (args.isEmpty || isLcmForRest(arg, args.toList)) arg
    else args.fold(arg * args.product)((acc, current) =>
      if (isLcmForRest(current, args.toList :+ arg)) current
      else acc
    )
  }

  // task 2 - gcd

  /**
   * Improved version of gcd function with 1..n arguments.
   *
   * @param arg  at least one argument requires
   * @param args 0..n rest of the arguments
   * @return greatest common divisor of agrs
   */
  def gcd(arg: Int, args: Int*): Int = {
    (args.toList :+ arg).map { current =>
      (1 to current).foldLeft(Set(current)) { (acc, div) =>
        if (current % div == 0) acc concat Set(div)
        else acc
      }
    }.reduce(_ intersect _).max
  }
}
