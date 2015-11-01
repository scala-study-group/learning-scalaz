package day10

import scala.language.implicitConversions // eyeroll
import scala.language.higherKinds

import scalaz._
import Scalaz._

object ReaderEx {

  val timesTwo: Reader[Int,Int] =
    Reader { x: Int => x * 2 }

  val plusTen: Reader[Int,Int] =
    Reader { x: Int => x + 10 }

  val addStuff: Reader[Int,Int] =
    for {
      a <- timesTwo
      b <- plusTen
      c  = a + b
    } yield c

  val x: Int = addStuff.run(3) // 19

}

object ReaderOptionEx {

  val timesTwo: Reader[Int,Option[Int]] =
    Reader { x: Int => Some(x * 2) }

  val plusTen: Reader[Int,Option[Int]] =
    Reader { x: Int => Some(x + 10) }

  val addStuff: Reader[Int,Option[Int]] =
    for {
      ao <- timesTwo
      bo <- plusTen
      co  = for {
              a <- ao
              b <- bo
              c  = a + b
            } yield c
    } yield co

  val xo: Option[Int] = addStuff.run(3) // Some(19)

}

object ReaderTEx {

  import Kleisli.kleisli
  import Kleisli.kleisliMonadTrans
  def ReaderT[V[_],T,U] = kleisli[V, T, U](_)

  val timesTwo: ReaderT[Option,Int,Int] =
    kleisli { x: Int => Some(x * 2) }

  val plusTen: Reader[Int,Int] =
    Reader { x: Int => x + 10 }

  val idToOption: Id ~> Option =
    new NaturalTransformation[Id, Option] {
      def apply[A](fa: Id[A]): Option[A] = Some(fa)
    }

  val addStuff: ReaderT[Option,Int,Int] =
    for {
      a <- timesTwo
      b <- kleisliMonadTrans.hoist(idToOption).apply(plusTen)
      c  = a + b
    } yield c

  val xo: Option[Int] = addStuff.run(3) // Some(19)

}

case class MyReader[E,A](run: E => A) {
  def map[B](f: A => B): MyReader[E,B] =
    MyReader { e: E => f(run(e)) }
  def flatMap[B](f: A => MyReader[E,B]): MyReader[E,B] =
    MyReader { e: E => f(run(e)).run(e) }
}

object MyReaderEx {

  val timesTwo: MyReader[Int,Int] =
    MyReader { x: Int => x * 2 }

  val plusTen: MyReader[Int,Int] =
    MyReader { x: Int => x + 10 }

  val addStuff: MyReader[Int,Int] =
    for {
      a <- timesTwo
      b <- plusTen
      c  = a + b
    } yield c

  val x: Int = addStuff.run(3) // 19

}

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] =
    this match {
      case MyNone => MyNone
      case MySome(a) => MySome(f(a))
    }
  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    this match {
      case MyNone => MyNone
      case MySome(a) => f(a)
    }
}

object MyOption {
  implicit val monad: Monad[MyOption] =
    new Monad[MyOption] {
      def point[A](a: => A): MyOption[A] = MySome(a)
      def bind[A, B](fa: MyOption[A])(f: A => MyOption[B]): MyOption[B] =
        fa match {
          case MyNone => MyNone
          case MySome(a) => f(a)
        }
    }
}

case class MySome[A](a: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]

object MyReaderMyOptionEx {

  val timesTwo: MyReader[Int,MyOption[Int]] =
    MyReader { x: Int => MySome(x * 2) }

  val plusTen: MyReader[Int,MyOption[Int]] =
    MyReader { x: Int => MySome(x + 10) }

  val addStuff: MyReader[Int,MyOption[Int]] =
    for {
      ao <- timesTwo
      bo <- plusTen
      co  = for {
              a <- ao
              b <- bo
              c  = a + b
            } yield c
    } yield co

  val xo: MyOption[Int] = addStuff.run(3) // MySome(19)

}

case class MyReaderT[F[_],E,A](run: E => F[A]) {
  def map[B](f: A => B)(implicit m: Monad[F]): MyReaderT[F,E,B] =
    MyReaderT { e: E => run(e).map(f) }
  def flatMap[B](f: A => MyReaderT[F,E,B])(implicit m: Monad[F]): MyReaderT[F,E,B] =
    MyReaderT { e: E => run(e).map(f).flatMap(r => r.run(e)) }
}

object MyReaderT {

  def hoist[M[_],N[_],E](f: M ~> N): MyReaderT[M,E,?] ~> MyReaderT[N,E,?] =
      new NaturalTransformation[MyReaderT[M,E,?], MyReaderT[N,E,?]] {
        def apply[A](rm: MyReaderT[M,E,A]): MyReaderT[N,E,A] =
          MyReaderT(e => f(rm.run(e)))
      }

}

object MyReaderTEx {

  val timesTwo: MyReaderT[MyOption,Int,Int] =
    MyReaderT { x: Int => MySome(x * 2) }

  val plusTen: MyReaderT[Id,Int,Int] =
    MyReaderT[Id,Int,Int] { x: Int => x + 10 }

  val idToMyOption: Id ~> MyOption =
    new NaturalTransformation[Id, MyOption] {
      def apply[A](fa: Id[A]): MyOption[A] = MySome(fa)
    }

  val addStuff: MyReaderT[MyOption,Int,Int] =
    for {
      a <- timesTwo
      b <- MyReaderT.hoist(idToMyOption).apply(plusTen)
      c  = a + b
    } yield c

  val xo: MyOption[Int] = addStuff.run(3) // MySome(19)

}

object Main extends App {

  println()

  // Scalaz Reader

  println("Reader example")
  println(ReaderEx.x) // 19
  println()

  println("ReaderOption example")
  println(ReaderOptionEx.xo) // Some(19)
  println()

  println("ReaderT example")
  println(ReaderTEx.xo) // Some(19)
  println()

  // My Reader

  println("MyReader example")
  println(MyReaderEx.x) // 19
  println()

  println("MyReaderMyOption example")
  println(MyReaderMyOptionEx.xo) // Some(19)
  println()

  println("MyReaderT example")
  println(MyReaderTEx.xo) // Some(19)
  println()

}
