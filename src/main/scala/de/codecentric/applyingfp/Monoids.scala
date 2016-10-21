package de.codecentric.applyingfp

import cats.instances.function._
import cats.instances.future._
import cats.instances.map._
import cats.instances.tuple._
import cats.kernel.Monoid
import cats.syntax.semigroup._

import scala.concurrent._
import scala.concurrent.duration._

object MonoidsExamples extends App {
  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val intPlus: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0
    override def combine(x: Int, y: Int): Int = x + y
  }

  implicit val string: Monoid[String] = new Monoid[String] {
    override def empty: String = ""
    override def combine(x: String, y: String): String = x + y
  }

  val foo: Int => Future[Map[Int,(Int,String)]] = x => Future(Map(x -> (x*2,x.toString)))
  val bar: Int => Future[Map[Int,(Int,String)]] = x => Future(Map(x -> (x/2,x.toString.reverse)))

  val foobar = foo |+| bar

  println(Await.result(foobar(42), Duration.Inf))
}
