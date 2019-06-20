package io.ctek.interview.dataeng

import org.specs2.Specification
import org.specs2.specification.core.SpecStructure

import scala.util.Success

class MathServiceSpec extends Specification {
  def is: SpecStructure = sequential ^
    s2"""
      MathService
      =============

      gcd
        should return correct result for one value $singleValue
        should return error single value of zero $singleValueZero
        should return correct result for two value $twoValue
        should return correct result for five value $fiveValues
        should return error result for null value $nullValue
        should return error result for empty set $noValue
        should return correct result for two mixed sign values $twoMixedSignValues
        should return correct result for positive number and zero $twoValuesPositiveAndZero
        should return correct result for negative number and zero $twoValuesNegativeAndZero
        should return error result for single negative value $singleNegativeValue
        should return error result for single integer min value $singleValueIntegerMinimum
        should return error result for 0 and integer min value $zeroAndIntegerMinimumValues
    """

  val mathService = new MathService

  def singleValue = {
    val dataEnv = DataEnvironment(Set(24))
    mathService.gcd.run(dataEnv) must beEqualTo(
      Success(Right(24))
    )
  }
  def singleValueZero = {
    val dataEnv = DataEnvironment(Set(0))
    mathService.gcd.run(dataEnv) must beEqualTo(
      Success(Left("gcd of 0 is undefined"))
    )
  }
  def twoValue = {
    val dataEnv = DataEnvironment(Set(14, 7))
    mathService.gcd.run(dataEnv) must beEqualTo(
      Success(Right(7))
    )
  }
  def fiveValues = {
    val dataEnv = DataEnvironment(Set(25, 35, 75, 145, 90))
    mathService.gcd.run(dataEnv) must beEqualTo(
      Success(Right(5))
    )
  }

  def nullValue = {
    val dataEnv = DataEnvironment(null)
    mathService.gcd.run(dataEnv) must beEqualTo(
      Success(Left("gcd of null set is undefined"))
    )
  }
  def noValue = {
    val dataEnv = DataEnvironment(Set.empty[Int])
    mathService.gcd.run(dataEnv) must beEqualTo(
      Success(Left("gcd of empty set is undefined"))
    )
  }
  def twoMixedSignValues = {
    val dataEnv = DataEnvironment(Set(-15, 25))
    mathService.gcd.run(dataEnv) must beEqualTo(
      Success(Right(5))
    )
  }
  def twoValuesPositiveAndZero = {
    val dataEnv = DataEnvironment(Set(15, 0))
    mathService.gcd.run(dataEnv) must beEqualTo(
      Success(Right(15))
    )
  }
  def twoValuesNegativeAndZero = {
    val dataEnv = DataEnvironment(Set(-15, 0))
    mathService.gcd.run(dataEnv) must beEqualTo(
      Success(Right(15))
    )
  }
  def singleNegativeValue = {
    val dataEnv = DataEnvironment(Set(-4))
    mathService.gcd.run(dataEnv) must beEqualTo(
      Success(Right(4))
    )
  }
  def singleValueIntegerMinimum = {
    val dataEnv = DataEnvironment(Set(Int.MinValue))
    mathService.gcd.run(dataEnv) must beEqualTo(
      Success(Left("gcd of set where expected result is abs(Int.MinValue) is undefined"))
    )
  }
  def zeroAndIntegerMinimumValues = {
    val dataEnv = DataEnvironment(Set(0, Int.MinValue))
    mathService.gcd.run(dataEnv) must beEqualTo(
      Success(Left("gcd of set where expected result is abs(Int.MinValue) is undefined"))
    )
  }
  def integerMinimumAndPositiveValue = {
    val dataEnv = DataEnvironment(Set(Int.MinValue, 14))
    mathService.gcd.run(dataEnv) must beEqualTo(
      Success(Right(1))
    )
  }
}
