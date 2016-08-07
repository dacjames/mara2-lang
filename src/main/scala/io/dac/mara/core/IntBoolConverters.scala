package io.dac.mara.core

/**
  * Created by dcollins on 8/6/16.
  * A dirty sin!
  */
object IntBoolConverters {
  implicit def int2bool(x: Int): Boolean = if (x == 0) false else true
  implicit def bool2int(x: Boolean): Int = if (x) { 1 } else { 0 }
}
