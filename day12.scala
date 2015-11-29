package day12

import scala.language.implicitConversions // eyeroll
import scala.language.higherKinds

import scalaz._
import Scalaz._

object Origami extends App {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[A](head: A, tail: List[A]) extends List[A]

  def wrap[A](x: A): List[A] =
    Cons(x, Nil)

  def nil[A](list: List[A]): Boolean =
    list match {
      case Nil => true
      case Cons(_, _) => false
    }

  def foldL[A,B](f: A => B => B, z: B, bs: List[A]): B =
    bs match {
      case Nil => z
      case Cons(h, t) => f(h)(foldL(f, z, t))
    }

  ////////////////////////////////////////////////////////////////////
  // Exercise 3.2

  def mapL[A,B](f: A => B, xs: List[A]): List[B] =
    xs match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), mapL(f, t))
    }

  implicit class ListMap[A](val xs: List[A]) extends AnyVal {
    def map[B](f: A => B): List[B] = mapL(f, xs)
  }

  def appendL[A](xs: List[A], ys: List[A]): List[A] =
    xs match {
      case Nil => ys
      case Cons(h, t) => Cons(h, appendL(t, ys))
    }

  implicit class ListAppend[A](val xs: List[A]) extends AnyVal {
    def append(ys: List[A]): List[A] = appendL(xs, ys)
  }

  def concatL[A](xss: List[List[A]]): List[A] =
    xss match {
      case Nil => Nil
      case Cons(h, t) => appendL(h, concatL(t))
    }

  implicit class ListConcat[A](val xss: List[List[A]]) extends AnyVal {
    def concat: List[A] = concatL(xss)
  }

}
