package day5

import scala.language.implicitConversions // eyeroll
import scala.language.higherKinds

import scalaz._
import Scalaz._

import scalacheck.ScalazProperties._
import scalacheck.ScalazArbitrary._
import scalacheck.ScalaCheckBinding._
import org.scalacheck._

sealed trait Maybe[+A]
case object Nada extends Maybe[Nothing]
case class Just[A](a: A) extends Maybe[A]

object Maybe {

  implicit def equal[A: Equal]: Equal[Maybe[A]] =
    new Equal[Maybe[A]] {
      def equal(a: Maybe[A], b: Maybe[A]): Boolean = a == b
    }

  implicit val monad: Monad[Maybe] =
    new Monad[Maybe] {
      def point[A](a: => A): Maybe[A] = Just(a)
      def bind[A, B](fa: Maybe[A])(f: A => Maybe[B]): Maybe[B] =
        fa match {
          case Nada => Nada
          case Just(a) => f(a)
        }
    }

  val genMaybeInt: Gen[Maybe[Int]] =
    for {
      x <- Arbitrary.arbitrary[Int]
      a <- Gen.oneOf(Nada, Just(x))
    } yield a

  implicit val arbitraryMaybeInt: Arbitrary[Maybe[Int]] =
    Arbitrary(genMaybeInt)

  val genMaybeIntInt: Gen[Maybe[Int => Int]]=
    for {
      x <- Arbitrary.arbitrary[Int => Int]
      a <- Gen.oneOf(Nada, Just(x))
    } yield a

  implicit val arbitraryMaybeIntToInt: Arbitrary[Maybe[Int => Int]] =
    Arbitrary(genMaybeIntInt)

}

object Main extends App {

  // Bind operators
  println("## Bind operators")
  import Maybe.monad.monadSyntax._
  println(Just(21) >>= { x => Just(x*2) })

  // Monad laws
  println("## Monad laws")
  monad.laws[Maybe].check
  println("")
  println("")

}

