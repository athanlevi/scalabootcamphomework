package net.savinko
package _3_controll_structures

import scala.io.Source

object ControlStructuresHomework {

  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command

  object Command {

    final case class Divide(dividend: Double, divisor: Double) extends Command

    final case class Sum(numbers: List[Double]) extends Command

    final case class Average(numbers: List[Double]) extends Command

    final case class Min(numbers: List[Double]) extends Command

    final case class Max(numbers: List[Double]) extends Command

  }

  sealed trait Exception {
    val message: String
  }

  object Exception {
    private val prefix = "Error:"

    final case class InvalidNumberOfArguments(override val message: String = s"$prefix Invalid number of arguments.") extends Exception

    final case class UnsupportedOperation(override val message: String = s"$prefix Unsupported operation.") extends Exception

    final case class DivisionByZero(override val message: String = s"$prefix Division by zero.") extends Exception

  }

  // Adjust `Result` and `ChangeMe` as you wish - you can turn Result into a `case class` and remove the `ChangeMe` if
  // you think it is the best model for your solution, or just have other `case class`-es implement `Result`
  sealed trait Result

  object Result {

    final case class DivisionResult(divided: Double, divisor: Double) extends Result {
      override def toString: String = s"The ${Command.Divide.getClass.getSimpleName.dropRight(1)} of $divided by $divisor is ${divided / divisor}"
    }

    final case class SumResult(args: List[Double]) extends Result {
      override def toString: String = resultForArgs(args, args.sum, Command.Min.getClass.getSimpleName.dropRight(1))
    }

    final case class AverageResult(args: List[Double]) extends Result {
      override def toString: String = resultForArgs(args, args.max / args.size, Command.Min.getClass.getSimpleName.dropRight(1))
    }

    final case class MinResult(args: List[Double]) extends Result {
      override def toString: String = resultForArgs(args, args.min, Command.Min.getClass.getSimpleName.dropRight(1))
    }

    final case class MaxResult(args: List[Double]) extends Result {
      override def toString: String = resultForArgs(args, args.max, Command.Min.getClass.getSimpleName.dropRight(1))
    }

    private def resultForArgs(args: List[Double], result: Double, command: String): String =
      s"The $command of ${args.mkString(", ")} is $result."
  }

  def parseCommand(expression: String): Either[Exception, Command] = {
    expression.trim.replaceAll(" +", " ").split(" ").toList match {
      case _ :: Nil => Left(Exception.InvalidNumberOfArguments())
      case x :: xs => parseOperator(x, xs.map(_.toDouble))
    }
  }

  def parseOperator(op: String, args: List[Double]): Either[Exception, Command] = op.toLowerCase() match {
    case "divide" => args.tail.headOption.map { divisor =>
      if (divisor == 0) Left(Exception.DivisionByZero()) else Right(Command.Divide(args.head, divisor))
    }.getOrElse(Left(Exception.InvalidNumberOfArguments()))
    case "sum" => Right(Command.Sum(args))
    case "min" => Right(Command.Min(args))
    case "max" => Right(Command.Max(args))
    case "average" => Right(Command.Average(args))
    case _ => Left(Exception.UnsupportedOperation())
  }

  def calculate(command: Command): Either[Exception, Result] = command match {
    case Command.Divide(dividend, divisor) => Right(Result.DivisionResult(dividend, divisor))
    case Command.Sum(args) => Right(Result.SumResult(args))
    case Command.Average(args) => Right(Result.AverageResult(args))
    case Command.Min(args) => Right(Result.MinResult(args))
    case Command.Max(args) => Right(Result.MaxResult(args))
  }

  def renderResult(result: Result): String = result.toString

  def process(line: String): String = {
    val calculationResult = for {
      parsed <- parseCommand(line)
      calculated <- calculate(parsed)
    } yield calculated

    calculationResult match {
      case Left(exception) => exception.message
      case Right(result) => renderResult(result)
    }
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
