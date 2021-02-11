package net.savinko
package _4_data_srtucrutes

object DataStructures extends App {
  // hometask:
  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(nums: Vector[Int]): Vector[Int] =
    nums.foldLeft((0, Vector.empty[Int])) { (acc, current) => (acc._1 + current, acc._2 :+ acc._1 + current) }._2

  // https://leetcode.com/problems/shuffle-the-array
  def shuffle(nums: Vector[Int], n: Int): Vector[Int] =
    nums.take(n).foldLeft(Vector.empty[Int]) { (acc, current) => acc :+ current :+ nums(nums.indexOf(current) + n) }

  println(shuffle(Vector(2, 5, 1, 3, 4, 7), 3))
  println(shuffle(Vector(1, 2, 3, 4, 4, 3, 2, 1), 4))
  println(shuffle(Vector(1, 1, 2, 2), 2))

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(nums: Vector[Vector[Int]]): Int = nums.map(_.sum).max

  println(maximumWealth(Vector(Vector(1, 2, 3), Vector(3, 2, 1))))
  println(maximumWealth(Vector(Vector(1, 5), Vector(7, 3), Vector(3, 5))))
  println(maximumWealth(Vector(Vector(2, 8, 7), Vector(7, 1, 3), Vector(1, 9, 5))))

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies(candies: Vector[Int], extraCandies: Int): Vector[Boolean] =
    candies.map { currentCandies =>
      if (currentCandies + extraCandies < candies.max) false
      else true
    }

  println(kidsWithCandies(Vector(2, 3, 5, 1, 3), 3))
  println(kidsWithCandies(Vector(4, 2, 1, 1, 2), 1))
  println(kidsWithCandies(Vector(12, 1, 12), 10))

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  def maxWidthOfVerticalArea(points: Vector[(Int, Int)]): Option[Int] =
    points.headOption.map { _ =>
      points.map(_._1).sorted.zip(points.map(_._1).sorted.tail).map { case (a, b) => b - a }.max
    }

  println(maxWidthOfVerticalArea(Vector((8, 7), (9, 9), (7, 4), (9, 7))))
  println(maxWidthOfVerticalArea(Vector((3, 1), (9, 0), (1, 0), (1, 4), (5, 3), (8, 8))))
}
