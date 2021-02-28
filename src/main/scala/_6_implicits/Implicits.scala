package net.savinko
package _6_implicits

import scala.util.Try


object Task1 {

  final case class Money(amount: BigDecimal)

  // create Ordering instance for Money
  implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)
}

object Task2 extends App {

  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  // create Show instance for User
  implicit val showUser: Show[User] = (entity: User) => s"User: id=${entity.id}, name=${entity.name}."

  // create syntax for Show so i can do User("1", "Oleg").show
  implicit class ShowEntity(entity: User) {
    def show: String = showUser.show(entity)
  }

  val user: User = User("1", "Oleg")
  println(user.show)
}

object Task3 extends App {
  type Error = String

  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  object Parse {
    def apply[T](implicit instance: Parse[T]): Parse[T] = instance
  }

  // {id:1,name:Oleg}
  final case class User(id: String, name: String)

  // create Parse instance for User
  implicit val parseUser: Parse[User] = {
    case s if s.startsWith("{") && s.endsWith("}") => s.drop(1).dropRight(1).split(",") match {
      case Array(a, b) => Try {
        val id = a.split(":").last
        val name = b.split(":").last

        Right(User(id, name))
      }.get
      case _ => Left("Invalid param count.")
    }
    case _ => Left("Invalid string.")
  }

  // create syntax for Parse so i can do "lalala".parse[User] (and get an error because it is obviously not a User)
  implicit class ParseUser(maybeUser: String) {
    def parse[T](implicit i: Parse[T]): Either[Error, T] = i.parse(maybeUser)
  }

  println("lalala".parse[User])
  println("{id:asd123,name: Oleg}".parse)
}


object Task4 extends App {

  // design a typesafe equals so i can do a === b, but it won't compile if a and b are of different types

  trait Equality[T] {
    def eq(left: T, right: T): Boolean
  }

  object Equality {
    def apply[T](implicit instance: Equality[T]): Equality[T] = instance
  }

  implicit class EqualityAux[BIND: Equality](left: BIND) {
    def ===(right: BIND): Boolean = Equality[BIND].eq(left, right)
  }

  implicit val stringEquality: Equality[String] = (left: String, right: String) => left.equals(right)
  implicit val boolEquality: Equality[Boolean] = (left: Boolean, right: Boolean) => left.equals(right)

  println("12" === "12")
  println("12" === "1")
  println(false === false)
  println(false === true)
  //  println(false === "1")

  // define the typeclass (think of a method signature)
  // remember `a method b` is `a.method(b)`
}

object AdvancedHomework extends App {

  // create a typeclass for flatMap method
  trait CustomMonad[F[_]] {
    def customFlatMap[T, R](fa: F[T])(f: T => F[R]): F[R]
  }

  implicit val listFlatMap: CustomMonad[List] = new CustomMonad[List] {
    override def customFlatMap[T, R](fa: List[T])(f: T => List[R]): List[R] = fa.flatMap(f)
  }

  object CustomMonad {
    def apply[F[_]](implicit instance: CustomMonad[F]): CustomMonad[F] = instance
  }

  implicit class CustomMonadSyntax[T, F[_]](fa: F[T]) {
    def applyFlatMap[R](f: T => F[R])(implicit i: CustomMonad[F]): F[R] = i.customFlatMap(fa)(f)
  }

  println(List(1, 3, 5).applyFlatMap(a => List(a, a + 1)))
}

object TypeclassTask {

  // Why am I not a Typeclass? A: type class contains type constructor
  trait HashCode[T] {
    def hash(entity: T): Int
  }

  object HashCode {
    def apply[T](implicit instance: HashCode[T]): HashCode[T] = instance
  }

  implicit class HashCodeSyntax[A](x: A) {
    def hash(implicit entity: HashCode[A]): Int = entity.hash(x)
  }

  // make an instance for String
  val stringHash: HashCode[String] = (entity: String) => entity.hashCode

  // write "abc".hash to check everything
  println(stringHash.hash("123"))
}

