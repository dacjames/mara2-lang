package io.dac.mara

/**
  * Created by dcollins on 8/2/16.
  */
trait ArithmeticAlg[E] extends ExprAlg[E] {
  def plus(x: E, y: E): E
  def minus(x: E, y: E): E
  def times(x: E, y: E): E
  def divide(x: E, y: E): E
  def power(x: E, y: E): E
}
