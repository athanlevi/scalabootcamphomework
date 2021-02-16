package net.savinko
package _5_adt

import scala.collection.SortedSet

object ADT {

  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.

  // 1. Suit
  sealed trait Suit

  object Suit {

    final case object Clubs extends Suit

    final case object Hearts extends Suit

    final case object Spades extends Suit

    final case object Diamonds extends Suit

  }

  // 2. Rank
  sealed trait Rank

  object Rank {

    final case object Two extends Rank

    final case object Three extends Rank

    final case object Four extends Rank

    final case object Five extends Rank

    final case object Six extends Rank

    final case object Seven extends Rank

    final case object Eight extends Rank

    final case object Nine extends Rank

    final case object Ten extends Rank

    final case object Jack extends Rank

    final case object Queen extends Rank

    final case object King extends Rank

    final case object Ace extends Rank

  }

  // 3. Card
  final case class Card(suite: Suit, rank: Rank)

  // 4. Hand (Texas or Omaha)
  sealed trait Hand

  object Hand {

    final case class OmahaHand(cards: Set[Card]) extends Hand

    final case class TexasHand(cards: Set[Card]) extends Hand

    object OmahaHand {
      def apply(cards: Set[Card]): Either[PokerException, Hand] = cards match {
        case cards if cards.size == 5 => Right(new OmahaHand(cards))
        case _ => Left(PokerException.InvalidHand(s"Invalid ${getClass.getSimpleName} hand"))
      }
    }

    object TexasHand {
      def apply(cards: Set[Card]): Either[PokerException, Hand] = cards match {
        case cards if cards.size == 2 => Right(new TexasHand(cards))
        case _ => Left(PokerException.InvalidHand(s"Invalid ${getClass.getSimpleName} hand"))
      }
    }

  }

  final case class Board private(cards: Set[Card])

  object Board {
    def apply(cards: Set[Card]): Either[PokerException, Board] = cards match {
      case cards if cards.size == 5 => Right(new Board(cards))
      case _ => Left(PokerException.InvalidBoard())
    }
  }

  sealed trait Combination

  object Combination {

    final case object HighCard extends Combination

    final case object Pair extends Combination

    // etc
  }

  final case class TestCase(board: Board, hands: Set[Hand]) {
    def result(): Either[PokerException, TestResult] = ???
  }

  final case class TestResult(board: Board, hands: SortedSet[Hand])

  sealed trait PokerException {
    val message: String
  }

  object PokerException {

    final case class InvalidBoard(override val message: String = "Invalid board") extends PokerException

    final case class InvalidHand(override val message: String) extends PokerException

  }

}
