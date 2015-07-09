package day1

import scala.language.implicitConversions // eyeroll

import scalaz._
import Scalaz._

sealed trait Amount
case object One extends Amount
case object Couple extends Amount
case object Few extends Amount

trait Read[A] {
  def read(a: String): Option[A]
}

object Amount {

  implicit val equal: Equal[Amount] =
    new Equal[Amount] {
      def equal(a: Amount, b: Amount): Boolean = a == b
    }

  implicit val order: Order[Amount] =
    new Order[Amount] {
      def order(a: Amount, b: Amount): Ordering =
        (a, b) match {
          case (One, One)       => Ordering.EQ
          case (Couple, Couple) => Ordering.EQ
          case (Few, Few)       => Ordering.EQ
          case (_, One)         => Ordering.GT
          case (Few, _)         => Ordering.GT
          case (One, _)         => Ordering.LT
          case (_, Few)         => Ordering.LT
        }
    }

  implicit val show: Show[Amount] =
    new Show[Amount] {
      override def shows(a: Amount): String = a.toString
    }

  implicit val read: Read[Amount] =
    new Read[Amount] {
      def read(a: String): Option[Amount] =
        a match {
          case "One"    => Some(One)
          case "Couple" => Some(Couple)
          case "Few"    => Some(Few)
          case _        => None
        }
    }

  implicit val enum: Enum[Amount] =
    new Enum[Amount] {
      def pred(a: Amount): Amount =
        a match {
          case One    => Few
          case Couple => One
          case Few    => Couple
        }
      def succ(a: Amount): Amount =
        a match {
          case One    => Couple
          case Couple => Few
          case Few    => One
        }
      def order(a: Amount, b: Amount): Ordering =
        Amount.order.order(a, b)
    }

}

trait Truthy[A] {
  def truthy(x: A): Boolean
}

object Truthy {

  def apply[A](f: A => Boolean): Truthy[A] =
    new Truthy[A] {
      def truthy(a: A) = f(a)
    }

  implicit val intTruthy: Truthy[Int] = Truthy({ x => x != 0})

  implicit def listTruthy[A]: Truthy[List[A]] = Truthy({ x => !x.isEmpty })

  def truthy[A: Truthy](a: A): Boolean =
    implicitly[Truthy[A]].truthy(a)

  def truthyIf[A: Truthy, B, C](cond: A)(ifyes: => B)(ifno: => C) =
    if (implicitly[Truthy[A]].truthy(cond)) ifyes else ifno

  implicit class TruthyOps[A: Truthy](a: A) {
    def truthy: Boolean = implicitly[Truthy[A]].truthy(a)
  }

}

trait Add[A] {
  def add(a: A, b: A): A
}

object Add {

  implicit val intAdd: Add[Int] =
    new Add[Int] {
      def add(a: Int, b: Int): Int = a + b
    }

  def add[A: Add](a: A, b: A): A =
    implicitly[Add[A]].add(a, b)

  implicit class AddInt(a: Int) {
    def add(b: Int): Int = a + b
  }

  implicit class AddOp[A: Add](a: A) {
    def add(b: A): A = implicitly[Add[A]].add(a, b)
  }

}

object Main extends App {

  val one: Amount = One
  val couple: Amount = Couple
  val few: Amount = Few

  // Equal
  assert(few === few)
  assert(couple =/= few)

  // Order
  assert(one lt couple)
  assert(few eq few)

  // Show
  assert(couple.show === "Couple")

  // Read
  assert(implicitly[Read[Amount]].read("Couple") === Some(Couple))

  // Enum
  assert(few.succ === one)
  assert((one |-> few) === List(one, couple, few))

  // Truthy
  assert(Truthy.truthy(1))
  assert(!Truthy.truthy(Nil: List[Nothing]))
  assert(Truthy.truthyIf(1)("yes")("no") == "yes")

  import Truthy.truthy
  import Truthy.truthyIf
  import Truthy.TruthyOps

  assert(truthy(1))
  assert(!truthy(Nil.asInstanceOf[List[Int]]))
  assert(truthyIf(1)("yes")("no") == "yes")

  assert(1.truthy)

  // Add
  assert(Add.add(37, 5) === 42)

  {
    import Add.AddInt
    assert((37 add 5) === 42)
  }

  {
    import Add.AddOp
    assert((37 add 5) === 42)
  }

}

