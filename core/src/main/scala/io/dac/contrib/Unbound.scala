package io.dac.contrib

import cats._
import cats.data._
import cats.free._

import scala.collection.mutable

/**
  * Created by dcollins on 11/20/16.
  */


case class StreamIdentifier(value: String) extends AnyVal {
  override def toString = s"«${value.toString}»"
}
case class Stream(id: StreamIdentifier, name: String)
object Stream {
  implicit def streamIdentifier(s: String): StreamIdentifier = StreamIdentifier(s)
}


object Unbound {

  object Crud {
    sealed trait Command[+A]
    case class Create(name: String) extends Command[Stream]
    case class Read(id: StreamIdentifier) extends Command[Stream]
    case class Update(stream: Stream, name: String) extends Command[Stream]
    case class Delete(stream: Stream) extends Command[Unit]

    class Dsl[F[_]](implicit I: Inject[Command, F]) {

      private[this] def inject[A](cmd: Command[A]): Free[F, A] =
        Free.inject[Command, F](cmd)

      def create(name: String): Free[F, Stream] =
        inject(Create(name))

      def read(id: StreamIdentifier): Free[F, Stream] =
        inject(Read(id))

      def update(stream: Stream, name: String): Free[F, Stream] =
        inject(Update(stream, name))

      def delete(stream: Stream): Free[F, Unit] =
        inject(Delete(stream))
    }

    object Dsl {
      implicit def crud[F[_]](implicit I: Inject[Command, F]): Dsl[F] = new Dsl[F]
    }
  }

  object Search {

    sealed trait Command[+A]
    case class ByName(name: String) extends Command[Seq[Stream]]
    case class ById(needle: String) extends Command[Seq[Stream]]

    class Dsl[F[_]](implicit I: Inject[Command, F]) {

      private[this] def inject[A](cmd: Command[A]): Free[F, A] =
        Free.inject[Command, F](cmd)

      def searchByName(name: String): Free[F, Seq[Stream]] =
        inject(ByName(name))

      def searchById(id: String): Free[F, Seq[Stream]] =
        inject(ById(id))
    }

    object Dsl {
      implicit def search[F[_]](implicit I: Inject[Command, F]): Dsl[F] = new Dsl[F]
    }
  }

  type Command[A] = Coproduct[Crud.Command, Search.Command, A]

}

object InMemory {
  import Unbound._


  class CrudInterpreter(state: mutable.Map[StreamIdentifier, Stream]) extends (Crud.Command ~> Id) {
    import Crud._

    private[this] def genId: StreamIdentifier =
      StreamIdentifier(java.util.UUID.randomUUID.toString)

    override def apply[A](fa: Command[A]) = fa match {
      case Create(name) => {
        val stream = Stream(genId, name)
        state += (stream.id -> stream)
        println(s"Created: ${stream}")
        stream
      }
      case Read(id: StreamIdentifier) => {
        val stream = state(id)
        println(s"Read: ${stream}")
        stream
      }
      case Update(stream: Stream, name: String) => {
        val newStream = state(stream.id).copy(name = name)
        state += (stream.id -> newStream)
        println(s"Updated: ${stream} -> ${newStream}")
        newStream
      }
      case Delete(stream: Stream) => {
        state -= stream.id
        println(s"Deleted: ${stream} -> ()")
        ()
      }
    }
  }

  class SearchInterpreter(state: mutable.Map[StreamIdentifier, Stream]) extends (Search.Command ~> Id) {
    import Search._
    override def apply[A](fa: Command[A]) = fa match {
      case ByName(needle) => {
        val streams = state.values.filter(s => s.name.contains(needle)).toVector
        println(s"Search By Name: ${needle} -> ${streams}")
        streams
      }
      case ById(needle) => {
        val streams = state.values.filter(s => s.id.value.contains(needle)).toVector
        println(s"Search By Id: ${needle} -> ${streams}")
        streams
      }
    }
  }

  private[this] def makeInterpreter(state: mutable.Map[StreamIdentifier, Stream]): Command ~> Id =
    new CrudInterpreter(state) or new SearchInterpreter(state)

  def interpreter: Command ~> Id = makeInterpreter(mutable.Map.empty)
}



object UnboundExample extends App {
  import Unbound._

  def program(implicit C: Crud.Dsl[Command], S: Search.Dsl[Command]): Free[Command, Stream] = {
    import C._
    import S._

    for {
      s1 <- create("dac.dummy")
      _ <- create("dac.dummy2")
      s2 <- update(s1, "dac.renamed")
      s3 <- read(s1.id)
      ss4 <- searchByName("dac")
      ss5 <- searchById("a")
      _ <- delete(s3)
    } yield ss4.head
  }

  program.foldMap(InMemory.interpreter)
}
