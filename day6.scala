package day6

import scala.language.implicitConversions // eyeroll
import scala.language.higherKinds

import scalaz._
import Scalaz._

object WriterEx1 {

  import scala.language.postfixOps

  def logNumber(x: Int): Writer[List[String], Int] =
    x.set(List("Got number: " + x.shows))

  def multWithLog: Writer[List[String], Int] = for {
    a <- logNumber(3)
    b <- logNumber(5)
  } yield a * b

  val result: (List[String], Int) = multWithLog run
  // (List(Got number: 3, Got number: 5),15)

}

object WriterEx2 {

  import scala.language.postfixOps

  val log: Writer[List[String], Unit] =
    WriterT.writer((Nil, ()))

  def set(x: String): Writer[List[String], Unit] =
    WriterT.writer((List(x), ()))

  def multWithLog: Writer[List[String], Int] = for {
    _ <- log
    a  = 3
    _ <- set("Got number: " + a)
    b  = 5
    _ <- set("Got number: " + b)
  } yield a * b

  val result: (List[String], Int) = multWithLog run
  // (List(Got number: 3, Got number: 5),15)

}

object ReaderEx1 {

  val addStuff: Int => Int = for {
    a <- (_: Int) * 2
    b <- (_: Int) + 10
  } yield a + b

  val x: Int = addStuff(3) // 19

}

object ReaderEx2 {

  def get[A]: A => A = identity

  val addStuff: Int => Int = for {
    e <- get[Int]
    a  = e * 2
    b  = e + 10
  } yield a + b

  val x: Int = addStuff(3) // 19

}

object Main extends App {

  println()
  println("Writer examples")
  println(WriterEx1.result) // (List(Got number: 3, Got number: 5),15)
  println(WriterEx2.result) // (List(Got number: 3, Got number: 5),15)
  println()

  println()
  println("Reader examples")
  println(ReaderEx1.x) // 19
  println(ReaderEx2.x) // 19
  println()

}

