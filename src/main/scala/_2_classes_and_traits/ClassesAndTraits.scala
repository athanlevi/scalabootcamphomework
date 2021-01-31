package net.savinko
package _2_classes_and_traits

object ClassesAndTraits extends App {

  // copy pasted from lectures and redesigned interfaces
  sealed trait Bounded {
    def minX: Double

    def maxX: Double

    def minY: Double

    def maxY: Double
  }

  sealed trait Bounded3D extends Bounded {
    def minZ: Double

    def maxZ: Double
  }

  sealed trait Located extends Bounded {
    def x: Double

    def y: Double
  }

  sealed trait Located3D extends Located with Bounded3D {
    def z: Double
  }

  sealed trait Shape extends Located {
    def area: Double
  }

  sealed trait Shape3D extends Located3D {
    def surfaceArea: Double

    def volume: Double
  }

  sealed trait MovableShape extends Shape {
    def move(dx: Double, dy: Double): MovableShape
  }

  sealed trait MovableShape3D extends Shape3D {
    def move(dx: Double, dy: Double, dz: Double): MovableShape3D
  }

  sealed trait Point2DCoordinates {
    def points: List[(Double, Double)]
  }

  sealed trait Point3DCoordinates {
    def points: List[(Double, Double, Double)]

    def movePoints(dx: Double, dy: Double, dz: Double): Set[(Double, Double, Double)] =
      points.map { case (x, y, z) => (x + dx, y + dy, z + dz) }.toSet
  }

  // 2d triangle and square

  final case class Triangle2D(_points: Set[(Double, Double)]) extends MovableShape
    with Point2DCoordinates {

    require(points.size == 3)

    override def points: List[(Double, Double)] = _points.toList

    override def x: Double = points.map(_._1).sum / points.size

    override def y: Double = points.map(_._2).sum / points.size

    override def minX: Double = points.map(_._1).min

    override def maxX: Double = points.map(_._1).max

    override def minY: Double = points.map(_._2).min

    override def maxY: Double = points.map(_._2).max

    override def move(dx: Double, dy: Double): MovableShape = {
      Triangle2D(points.map { case (x: Double, y: Double) => (x + dx, y + dy) }.toSet)
    }

    /*
       1/2 *  | x1 - x3   y1 - y3 |
              | x2 - x3   y2 - y3 |
     */
    override def area: Double = 0.5 * Math.abs(
      (points.head._1 - points(2)._1) * (points(1)._2 - points(2)._2) - (points.head._2 - points(2)._2) * (points(1)._1 - points(2)._1)
    )
  }

  final case class Square(centerX: Double, centerY: Double, length: Double) extends MovableShape {
    override def x: Double = centerX

    override def y: Double = centerY

    override def minX: Double = x - length / 2

    override def maxX: Double = x + length / 2

    override def minY: Double = y - length / 2

    override def maxY: Double = y + length / 2

    override def move(dx: Double, dy: Double): MovableShape = Square(x + dx, y + dy, length)

    override def area: Double = Math.pow(length, 2)
  }

  val t = Triangle2D(Set((1, 1), (1, 4), (4, 1)))
  println(t.move(2, 2).area)

  // point, sphere, cube, cuboid, 3D triangle
  final case class Point3D(override val x: Double, override val y: Double, override val z: Double) extends MovableShape3D {

    override def move(dx: Double, dy: Double, dz: Double): MovableShape3D = Point3D(x + dx, y + dy, z + dz)

    override def surfaceArea: Double = 0

    override def volume: Double = 0

    override def minZ: Double = z

    override def maxZ: Double = z

    override def minX: Double = x

    override def maxX: Double = x

    override def minY: Double = y

    override def maxY: Double = y
  }


  final case class Sphere3D(override val x: Double, override val y: Double, override val z: Double, val radius: Double) extends MovableShape3D {
    override def move(dx: Double, dy: Double, dz: Double): MovableShape3D =
      Sphere3D(dx + x, dy + y, dz + z, radius)

    override def surfaceArea: Double = 4 * Math.PI * Math.pow(radius, 2)

    override def volume: Double = 4 * Math.PI * Math.pow(radius, 3) / 3

    override def minZ: Double = z - radius

    override def maxZ: Double = z + radius

    override def minX: Double = x - radius

    override def maxX: Double = x + radius

    override def minY: Double = y - radius

    override def maxY: Double = y + radius
  }

  final case class Cube(override val x: Double, override val y: Double, override val z: Double, length: Double) extends MovableShape3D {
    override def move(dx: Double, dy: Double, dz: Double): MovableShape3D = Cube(x + dx, y + dy, z + dz, length)

    override def surfaceArea: Double = 6 * Math.pow(length, 2)

    override def volume: Double = Math.pow(length, 3)

    override def minZ: Double = z - length / 2

    override def maxZ: Double = z + length / 2

    override def minX: Double = x - length / 2

    override def maxX: Double = x + length / 2

    override def minY: Double = y - length / 2

    override def maxY: Double = y + length / 2
  }

  final case class Cuboid(_points: Set[(Double, Double, Double)]) extends MovableShape3D with Point3DCoordinates {
    require(_points.size == 8)

    override def move(dx: Double, dy: Double, dz: Double): MovableShape3D = Cuboid(movePoints(dx, dy, dz))

    override def points: List[(Double, Double, Double)] = _points.toList

    override def surfaceArea: Double = ??? // not implemented, requires integrals and better design

    override def volume: Double = ??? // not implemented, requires integrals and better design

    override def x: Double = points.map(_._1).sum / points.size

    override def y: Double = points.map(_._2).sum / points.size

    override def z: Double = points.map(_._3).sum / points.size

    override def minX: Double = points.map(_._1).min

    override def maxX: Double = points.map(_._1).max

    override def minY: Double = points.map(_._2).min

    override def maxY: Double = points.map(_._2).max

    override def minZ: Double = points.map(_._3).min

    override def maxZ: Double = points.map(_._3).max
  }

  final case class Triangle3D(_points: Set[(Double, Double, Double)]) extends MovableShape3D with Point3DCoordinates {
    override def move(dx: Double, dy: Double, dz: Double): MovableShape3D = Triangle3D(movePoints(dx, dy, dz))

    override def points: List[(Double, Double, Double)] = _points.toList

    override def surfaceArea: Double = 0

    override def volume: Double = 0

    override def x: Double = points.map(_._1).sum / points.size

    override def y: Double = points.map(_._2).sum / points.size

    override def z: Double = points.map(_._3).sum / points.size

    override def minX: Double = points.map(_._1).min

    override def maxX: Double = points.map(_._1).max

    override def minY: Double = points.map(_._2).min

    override def maxY: Double = points.map(_._2).max

    override def minZ: Double = points.map(_._3).min

    override def maxZ: Double = points.map(_._3).max
  }
}
