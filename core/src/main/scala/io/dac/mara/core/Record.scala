package io.dac.mara.core

/**
  * Created by dcollins on 8/20/16.
  */

sealed trait Record[A] {
  def get(key: Record.Key): Option[A]
  def apply(key: Record.Key): A = this.get(key).get
}

object Record {
  sealed trait Key
  case class IntKey(x: Int) extends Key
  case class StringKey(x: String) extends Key

  private[this] def nonNegative(f: Int) =
    if ( f >= 0 ) Some(f) else { None }

  case class SeqRep[T](keys: Seq[Key],
                       values: Seq[T])
    extends Record[T] {

    override def get(key: Key): Option[T] = {
      val i = keys.indexWhere(_ == key)
      nonNegative(i).map(values)
    }

    override def toString: String = {
      val inner = (keys zip values) map {
        case (k, v) => {
          val ks = k match { case IntKey(i) => i.toString case StringKey(s) => s }
          s"$ks: $v"
        }
      } mkString ", "

      s"Record($inner)"
    }
  }

  def apply[T](tags: (Key, T)*): Record[T] = {
    SeqRep(tags.map(_._1), tags.map(_._2))
  }


  implicit def int2tag[T](tag: (Int, T)): (Key, T) = (IntKey(tag._1), tag._2)
  implicit def string2tag[T](tag: (String, T)): (Key, T) = (StringKey(tag._1), tag._2)
  implicit def int2key: Int => Key = IntKey(_)
  implicit def string2key: String => Key = StringKey(_)
  implicit def ints2tags[T](tags: Seq[(Int, T)]): Seq[(Key, T)] = tags.map(int2tag(_))
  implicit def strings2tags[T](tags: Seq[(String, T)]): Seq[(Key, T)] = tags.map(string2tag(_))


}




