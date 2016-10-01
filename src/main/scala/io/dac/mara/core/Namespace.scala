package io.dac.mara.core

import io.dac.mara.core.MaraValue.ErrorValue

import scala.collection.mutable

/**
  * Created by dcollins on 10/1/16.
  */
trait Namespace {
  private val namespace: mutable.Map[String, Option[MaraValue]] = mutable.Map.empty

  private[this] def whenUnassigned(name: String) = ErrorValue(s"Unassigned variable ${name}")
  private[this] def whenUndeclared(name: String) = ErrorValue(s"Undeclared variable ${name}")

  def lookupValue(name: String): MaraValue = {
    namespace.get(name).map(_.getOrElse(whenUndeclared(name))) getOrElse whenUnassigned(name)
  }

  def declareValue(name: String) = {
    namespace += (name -> None)
  }

  def bindValue[V <: MaraValue](name: String, value: V): V = {
    namespace += (name -> Some(value))
    value
  }

  def unbindValue(name: String) = {
    namespace -= name
  }
}
