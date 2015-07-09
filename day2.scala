package day2 {

import scala.language.implicitConversions // eyeroll
import scala.language.higherKinds

import scalaz._
import Scalaz._

sealed trait Amount[A]
case class One[A](a: A) extends Amount[A]
case class Couple[A](a: A, b: A) extends Amount[A]
case class Few[A](a: A, b: A, c: A) extends Amount[A]

object Amount {

  implicit def equal[A]: Equal[Amount[A]] =
    new Equal[Amount[A]] {
      def equal(a: Amount[A], b: Amount[A]): Boolean = a == b
    }

  implicit val functor: Functor[Amount] =
    new Functor[Amount] {
      def map[A, B](fa: Amount[A])(f: A => B): Amount[B] =
        fa match {
          case One(a) => One(f(a))
          case Couple(a, b) => Couple(f(a), f(b))
          case Few(a, b, c) => Few(f(a), f(b), f(c))
        }
    }

  implicit val applicative: Applicative[Amount] =
    new Applicative[Amount] {
      def point[A](a: => A): Amount[A] = One(a)
      def ap[A, B](fa: => Amount[A])(f: => Amount[A => B]): Amount[B] =
        fa match {
          case One(a) =>
            f match {
              case One(g) => One(g(a))
              case Couple(g, _) => One(g(a))
              case Few(g, _, _) => One(g(a))
            }
          case Couple(a, b) =>
            f match {
              case One(g) => Couple(g(a), g(b))
              case Couple(g, _) => Couple(g(a), g(b))
              case Few(g, _, _) => Couple(g(a), g(b))
            }
          case Few(a, b, c) =>
            f match {
              case One(g) => Few(g(a), g(b), g(c))
              case Couple(g, _) => Few(g(a), g(b), g(c))
              case Few(g, h, i) => Few(g(a), h(b), i(c))
            }
        }
    }
}

object Main extends App {

  // Functor
  assert(((One(6): Amount[Int]) map { x: Int => x * 7 }) === One(42))
  assert(((One(6): Amount[Int]) ∘ { x: Int => x * 7 }) === One(42))

  // Functor of Function1
  val inc = (x: Int) => x + 1
  val timesTwo = (x: Int) => x * 2

  assert((inc ∘ timesTwo)(3) === 8)
  assert((timesTwo ∘ inc)(3) === 7)

  val amountTimesTwo = Functor[Amount].lift(timesTwo)
  assert(amountTimesTwo(Few(1,2,3)) === Few(2,4,6))

  // Apply
  assert((One(21): Amount[Int]) <*> One({ x: Int => x * 2 }) === One(42))

  assert((One(6): Amount[Int]) <* One(7) === One(6))
  assert((One(6): Amount[Int]) *> One(7) === One(7))

  // Applicative Style
  assert(^((One(6): Amount[Int]), One(7)) { _ * _ } === One(42))
  assert(((One(6): Amount[Int]) |@| One(7)) { _ * _ } === One(42))

  // Useful functions for Applicatives
  def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
    case Nil     => (Nil: List[A]).point[F]
    case x :: xs => (x |@| sequenceA(xs)) {_ :: _} 
  }
  assert(sequenceA[Amount, Int](List(One(2), One(3), One(7))) === One(List(2,3,7)))

}

}
