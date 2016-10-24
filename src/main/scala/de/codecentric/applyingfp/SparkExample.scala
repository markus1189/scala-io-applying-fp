package de.codecentric

import org.apache.spark._

import org.apache.spark.SparkContext._
import org.apache.spark._
import org.apache.spark.rdd._

import cats._
import cats.instances.tuple._
import cats.kernel.Monoid
import cats.kernel.instances.int._
import cats.kernel.instances.option._
import cats.syntax.semigroup._

// Value classes broken in 2.10: SI-7685
case class Min[A](value: A)  // extends AnyVal
case class Max[A](value: A)  // extends AnyVal
case class Prod[A](value: A) // extends AnyVal

object Min {
  implicit def minSemi[A: Order] = new Semigroup[Min[A]] {
    private val o = implicitly[Order[A]]
    def combine(x: Min[A], y: Min[A]): Min[A] = (x, y) match {
      case (Min(xx), Min(yy)) => Min(o.min(xx, yy))
    }
  }
}

object Max {
  implicit def maxSemi[A: Order] = new Semigroup[Max[A]] {
    private val o = implicitly[Order[A]]
    def combine(x: Max[A], y: Max[A]): Max[A] = (x, y) match {
      case (Max(xx), Max(yy)) => Max(o.max(xx, yy))
    }
  }
}

trait SparkSugar {
  implicit class MonoidRDD[T](val rdd: RDD[T]) {

    // avoid conflicts with fold/reduce etc
    def combine(implicit M: Monoid[T]): T =
      rdd.fold(M.empty)(M.combine(_,_))

  }
}

object SparkExample1 extends SparkSugar {
  def main(args: Array[String]) = {
    val file = "/home/markus/repos/clones/stack/README.md" // Should be some file on your system
    val conf = new SparkConf().setMaster("spark://nixos:7077").setAppName("spark-monoids")
    val sc: SparkContext = new SparkContext(conf)

    def expand(word: String) = {
      (Option(Max(word.length)), Option(Min(word.length)), word.length, 1)
    }

    val data = "Scala.io The Scala event in France".split("""\s+""")
    val words = sc.parallelize(data).flatMap(_.split("""\s+""")).map(expand)
    val z = Monoid.empty[(Option[Max[Int]],Option[Min[Int]],Int,Int)]

    val (max,min,chars,ws) = words.combine

    println(s"""Finished calculation:
               |  - max word length: $max
               |  - min word length: $min
               |  - total characters: $chars
               |  - total words: $words
               |  - average word length: ${chars/ws}
               |""".stripMargin)
  }
}

object SparkExample2 extends SparkSugar {
  def main(args: Array[String]) = {
    val file = "/home/markus/repos/clones/stack/README.md" // Should be some file on your system
    val conf = new SparkConf().setMaster("spark://nixos:7077").setAppName("spark-monoids")
    val sc: SparkContext = new SparkContext(conf)

    def expand(word: String) = {
      (Option(Max(word.length)), Option(Min(word.length)), word.length, 1)
    }

    val data = "Scala.io The Scala event in France".split("""\s+""")
    val words = sc.parallelize(data).flatMap(_.split("""\s+""")).map(expand)
    val z = Monoid.empty[(Option[Max[Int]],Option[Min[Int]],Int,Int)]

    val (max,min,chars,ws) = words.combine

    println(s"""Finished calculation:
               |  - max word length: $max
               |  - min word length: $min
               |  - total characters: $chars
               |  - total words: $words
               |  - average word length: ${chars/ws}
               |""".stripMargin)
  }
}
