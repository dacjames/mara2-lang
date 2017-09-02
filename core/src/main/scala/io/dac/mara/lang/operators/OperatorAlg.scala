package io.dac.mara.lang.operators

import io.dac.mara.core._

/**
  * Created by dcollins on 8/2/16.
  */
trait OperatorAlg[E] extends ExprAlg[E] {
  def plus(x: E, y: E): E = empty
  def minus(x: E, y: E): E = empty
  def times(x: E, y: E): E = empty
  def divide(x: E, y: E): E = empty
  def power(x: E, y: E): E = empty

  def lt(x: E, y: E): E = empty
  def gt(x: E, y: E): E = empty
  def lte(x: E, y: E): E = empty
  def gte(x: E, y: E): E = empty
  def ne(x: E, y: E): E = empty
  def eq(x: E, y: E): E = empty

  def and(x: E, y: E): E = empty
  def or(x: E, y: E): E = empty
  def not(x: E): E = empty
  def nand(x: E, y: E) = not(and(x, y))

  def base(x: E, y: E): E = empty
  def low(x: E, y: E): E = empty
  def medium(x: E, y: E): E = empty
  def high(x: E, y: E): E = empty

}

