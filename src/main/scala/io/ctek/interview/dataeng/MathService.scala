package io.ctek.interview.dataeng

import cats.data.{Kleisli, ReaderT}
import cats.instances.try_._

import scala.math.abs
import scala.util.Try

case class DataEnvironment(data: Set[Int])

class MathService {
  def gcd: ReaderT[Try, DataEnvironment, Either[String, Int]] = for {
    data <- retrieveData
  } yield data.map(_.foldLeft(0)(gcd))

  def gcd(a: Int, b: Int): Int = b match {
    case 0 => abs(a)
    case _ => gcd(b, a % b)
  }

  def retrieveData: ReaderT[Try, DataEnvironment, Either[String, Set[Int]]] = Kleisli {
    (env) =>
      Try {
        if (env.data == null) {
          Left("gcd of null set is undefined")
        } else if (env.data == Set()){
          Left("gcd of empty set is undefined")
        } else if (env.data == Set(0)){
          Left("gcd of 0 is undefined")
        } else if (env.data == Set(Int.MinValue) || env.data == Set(0, Int.MinValue)){
          Left("gcd of set where expected result is abs(Int.MinValue) is undefined")
        } else {
          Right(env.data)
        }
      }
  }
}
