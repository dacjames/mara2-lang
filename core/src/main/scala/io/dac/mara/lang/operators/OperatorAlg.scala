package io.dac.mara.lang.operators

import io.dac.mara.core._

/**
  * Created by dcollins on 8/2/16.
  */
trait OperatorAlg[E] extends ExprAlg[E] {
  def plus(x: E, y: E): E = ???
  def minus(x: E, y: E): E = ???
  def times(x: E, y: E): E = ???
  def divide(x: E, y: E): E = ???
  def power(x: E, y: E): E = ???

  def lt(x: E, y: E): E = ???
  def gt(x: E, y: E): E = ???
  def lte(x: E, y: E): E = ???
  def gte(x: E, y: E): E = ???
  def ne(x: E, y: E): E = ???
  def eq(x: E, y: E): E = ???

  def and(x: E, y: E): E = ???
  def or(x: E, y: E): E = ???
  def not(x: E): E = ???
  def nand(x: E, y: E) = not(and(x, y))

  def base(x: E, y: E): E = ???
  def low(x: E, y: E): E = ???
  def medium(x: E, y: E): E = ???
  def high(x: E, y: E): E = ???

}

