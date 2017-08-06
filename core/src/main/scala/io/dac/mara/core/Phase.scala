package io.dac.mara.core

abstract class Phase[A] {
  def key: Int
  def bottom: A = ???
}
