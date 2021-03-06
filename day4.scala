package day4

import scala.language.implicitConversions // eyeroll
import scala.language.higherKinds

import scalaz._
import Scalaz._

import scalacheck.ScalazProperties._
import scalacheck.ScalazArbitrary._
import scalacheck.ScalaCheckBinding._
import org.scalacheck._

sealed trait Amount[A]
case class One[A](a: A) extends Amount[A]
case class Couple[A](a: A, b: A) extends Amount[A]
case class Few[A](a: A, b: A, c: A) extends Amount[A]

object Amount {

  implicit def equal[A: Equal]: Equal[Amount[A]] =
    new Equal[Amount[A]] {
      def equal(a: Amount[A], b: Amount[A]): Boolean = a == b
    }

  implicit val functorAmount: Functor[Amount] =
    new Functor[Amount] {
      def map[A, B](fa: Amount[A])(f: A => B): Amount[B] =
        fa match {
          //case One(a: Int) => One(f.asInstanceOf[Int => B](42))
          case One(a) => One(f(a))
          case Couple(a, b) => Couple(f(a), f(b))
          case Few(a, b, c) => Few(f(a), f(b), f(c))
        }
    }

  implicit val applicativeAmount: Applicative[Amount] =
    new Applicative[Amount] {
      def point[A](a: => A): Amount[A] = Few(a, a, a)
      def ap[A, B](fa: => Amount[A])(f: => Amount[A => B]): Amount[B] =
        fa match {
          case One(a) =>
            f match {
              case One(g)       => One(g(a))
              case Couple(g, _) => One(g(a))
              case Few(g, _, _) => One(g(a))
            }
          case Couple(a, b) =>
            f match {
              case One(g)       => One(g(a))
              case Couple(g, h) => Couple(g(a), h(b))
              case Few(g, h, _) => Couple(g(a), h(b))
            }
          case Few(a, b, c) =>
            f match {
              case One(g)       => One(g(a))
              case Couple(g, h) => Couple(g(a), h(b))
              case Few(g, h, i) => Few(g(a), h(b), i(c))
            }
        }
    }

  val genAmountInt: Gen[Amount[Int]]=
    for {
      x <- Arbitrary.arbitrary[Int]
      y <- Arbitrary.arbitrary[Int]
      z <- Arbitrary.arbitrary[Int]
      a <- Gen.oneOf(One(x), Couple(x,y), Few(x,y,z))
    } yield a

  implicit val arbitraryAmountInt: Arbitrary[Amount[Int]] =
    Arbitrary(genAmountInt)

  val genAmountIntInt: Gen[Amount[Int => Int]]=
    for {
      x <- Arbitrary.arbitrary[Int => Int]
      y <- Arbitrary.arbitrary[Int => Int]
      z <- Arbitrary.arbitrary[Int => Int]
      a <- Gen.oneOf(One(x), Couple(x,y), Few(x,y,z))
    } yield a

  implicit val arbitraryAmountIntToInt: Arbitrary[Amount[Int => Int]] =
    Arbitrary(genAmountIntInt)

  implicit val semigroupAmountInt: Semigroup[Amount[Int]] =
    new Semigroup[Amount[Int]] {
      def append(f1: Amount[Int], f2: => Amount[Int]): Amount[Int] =
        f1 match {
          case One(a) =>
            f2 match {
              case One(g)       => One(g + a)
              case Couple(g, _) => One(g + a)
              case Few(g, _, _) => One(g + a)
            }
          case Couple(a, b) =>
            f2 match {
              case One(g)       => One(g + a)
              case Couple(g, h) => Couple(g + a, h + b)
              case Few(g, h, _) => Couple(g + a, h + b)
            }
          case Few(a, b, c) =>
            f2 match {
              case One(g)       => One(g + a)
              case Couple(g, h) => Couple(g + a, h + b)
              case Few(g, h, i) => Few(g + a, h + b, i + c)
            }
        }
    }

}

object Main extends App {

  // Functor laws
  println("## Functor laws")
  println("")
  functor.laws[Amount].check
  println("")

  // Applicative laws
  println("## Applicative laws")
  println("")
  applicative.laws[Amount].check
  println("")

  // Semigroup laws
  println("## Semigroup laws")
  println("")
  semigroup.laws[Amount[Int]].check
  println("")

}

