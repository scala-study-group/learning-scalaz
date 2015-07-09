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

  val gen: Gen[Amount[Int]]=
    for {
      x <- Arbitrary.arbitrary[Int]
      y <- Arbitrary.arbitrary[Int]
      z <- Arbitrary.arbitrary[Int]
      a <- Gen.oneOf(One(x), Couple(x,y), Few(x,y,z))
    } yield a

  implicit val arbitrary: Arbitrary[Amount[Int]] =
    Arbitrary(gen)

}

object Main extends App {

  // Functor
  functor.laws[Amount].check

}

