package io.dac.mara.core

/**
  * Created by dcollins on 8/20/16.
  */

sealed trait Record[A] {
  def get(key: Record.Key): Option[A]
  def apply(key: Record.Key): A = this.get(key).get

  def keys: Seq[Record.Key]
  def values: Seq[A]

  def under(that: Record[A]): Record[A]
  def over(that: Record[A]): Record[A] = that.under(this)
}

object Record {
  sealed trait Key
  case class IntKey(x: Int) extends Key
  case class StringKey(x: String) extends Key

  type Label = Option[String]


  private[this] def nonNegative(f: Int) =
    if ( f >= 0 ) Some(f) else { None }

  case class SeqRep[T](keys: Seq[Key],
                       values: Seq[T])
    extends Record[T] {

    override def get(key: Key): Option[T] = {
      val index = key match {
        case IntKey(i) => i
        case StringKey(s) => keys.indexWhere(_ == key)
      }
      nonNegative(index).map(values)
    }

    override def under(that: Record[T]): Record[T] =
      extend(that.asInstanceOf[SeqRep[T]])

    def extend(that: SeqRep[T]): Record[T] = {

      // Using a fold here to get map and filter at once
      // In mara, collections can implement map.filter
      val newKeys =
        this.keys.zipWithIndex.foldRight(Seq.empty[Key]) {
          case ((key, index), restkeys: Seq[Key]) =>
            if (index < that.keys.length ||
                that.keys.contains(key)) restkeys
            else restkeys :+ key

        } ++ that.keys

      val newValues = newKeys.map { k =>
        that.get(k).getOrElse(this(k))
      }

      Record(newKeys zip newValues: _*)
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

  object SeqRep {
    def apply[T](tags: (Key, T)*): Record[T] = {
      SeqRep(tags.map(_._1), tags.map(_._2))
    }
  }

  def apply[T](tags: (Key, T)*): Record[T] =
    SeqRep.apply(tags: _*)

  def construct[T](tags: (Key, T)*): Either[String, Record[T]] = {
    lazy val keys = tags.map(_._1)

    def outOfOrder(record: Record[T]) = {
      val bad = keys.zipWithIndex.collect {
        case (IntKey(i), p) if i != p => (i, p)
      }

      bad.length match {
        case 0 => Right(record)
        case 1 => Left(s"Out of order keys ${bad}")
      }
    }

    def intAfterString(record: Record[T]): Either[String, Record[T]] = {
      var isString = false
      for (k <- keys) {
        k match {
          case StringKey(_) =>
            isString = true

          case IntKey(_) =>
            if (isString) return Left(s"Int keys must proceed string keys")
        }
      }
      Right(record)
    }

    def keysAreSet(record: Record[T]): Either[String, Record[T]] = {
      var keySet = Set.empty[Key]
      for (k <- keys) {
        if (keySet.contains(k)) return Left(s"Record contains duplicate key: ${k}")
        else keySet += k
      }
      Right(record)
    }

    outOfOrder(apply(tags: _*)) flatMap intAfterString flatMap keysAreSet
  }


  implicit def int2tag[T](tag: (Int, T)): (Key, T) = (IntKey(tag._1), tag._2)
  implicit def string2tag[T](tag: (String, T)): (Key, T) = (StringKey(tag._1), tag._2)
  implicit def int2key: Int => Key = IntKey(_)
  implicit def string2key: String => Key = StringKey(_)
  implicit def ints2tags[T](tags: Seq[(Int, T)]): Seq[(Key, T)] = tags.map(int2tag(_))
  implicit def strings2tags[T](tags: Seq[(String, T)]): Seq[(Key, T)] = tags.map(string2tag(_))


  def throwImpossibleKeys(): Nothing =
    throw new IllegalStateException("Impossible! Record Keys must always be Ints or Strings")


}




