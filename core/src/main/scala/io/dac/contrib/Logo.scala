package io.dac.contrib

import cats._
import cats.data._
import cats.free._




case class Position(x: Double, y: Double, heading: Heading)
sealed trait Heading
object Heading {
  case class Left() extends Heading
  case class Right() extends Heading
  case class Up() extends Heading
  case class Down() extends Heading
}

object Computations {
  import Heading._

  def forward(p: Position, length: Int): Position =
    p.heading match {
      case Right() => p.copy(x = p.x + length)
      case Left() => p.copy(x = p.x - length)
      case Up() => p.copy(y = p.y + length)
      case Down() => p.copy(y = p.y - length)
    }

  def backward(p: Position, length: Int): Position =
    p.heading match {
      case Right() => p.copy(x = p.x - length)
      case Left() => p.copy(x = p.x + length)
      case Up() => p.copy(y = p.y - length)
      case Down() => p.copy(y = p.y + length)
    }

  def rotateRight(p: Position): Position =
    p.heading match {
      case Right() => p.copy(heading = Down())
      case Left() => p.copy(heading = Up())
      case Up() => p.copy(heading = Right())
      case Down() => p.copy(heading = Left())
    }

  def rotateLeft(p: Position): Position =
    p.heading match {
      case Right() => p.copy(heading = Up())
      case Left() => p.copy(heading = Down())
      case Up() => p.copy(heading = Left())
      case Down() => p.copy(heading = Right())
    }

}


object Logo {
  sealed trait Instruction[A]
  case class Forward(position: Position, length: Int) extends Instruction[Position]
  case class Backward(position: Position, length: Int) extends Instruction[Position]
  case class RotateLeft(position: Position) extends Instruction[Position]
  case class RotateRight(position: Position) extends Instruction[Position]
  case class ShowPosition(position: Position) extends Instruction[Unit]

  sealed trait PencilInstruction[A]
  case class PencilUp(position: Position) extends PencilInstruction[Unit]
  case class PencilDown(position: Position) extends PencilInstruction[Unit]

  object dsl {
    class Moves[F[_]](implicit I: Inject[Instruction, F]) {
      def forward(pos: Position, l: Int): Free[F, Position] =
        Free.inject[Instruction, F](Forward(pos, l))

      def backward(pos: Position, l: Int): Free[F, Position] =
        Free.inject[Instruction, F](Backward(pos, l))

      def left(pos: Position): Free[F, Position] =
        Free.inject[Instruction, F](RotateLeft(pos))

      def right(pos: Position): Free[F, Position] =
        Free.inject[Instruction, F](RotateRight(pos))

      def showPosition(pos: Position): Free[F, Unit] =
        Free.inject[Instruction, F](ShowPosition(pos))
    }

    object Moves {
      implicit def moves[F[_]](implicit I: Inject[Instruction, F]): Moves[F] = new Moves[F]
    }

    class PencilActions[F[_]](implicit I: Inject[PencilInstruction, F]) {
      def pencilUp(pos: Position) = Free.inject[PencilInstruction, F](PencilUp(pos))
      def pencilDown(pos: Position) = Free.inject[PencilInstruction, F](PencilDown(pos))
    }

    object PencilActions {
      implicit def pencilActions[F[_]](implicit I: Inject[PencilInstruction, F]): PencilActions[F] = new PencilActions[F]
    }

  }

  object PenInterpreterId extends (PencilInstruction ~> Id) {
    def apply[A](fa: PencilInstruction[A]): Id[A] = fa match {
      case PencilUp(p) => println(s"stop drawing at position $p")
      case PencilDown(p) => println(s"start drawing at position $p")
    }
  }

  object InterpreterId extends (Instruction ~> Id) {
    import Computations._
    override def apply[A](fa: Instruction[A]): Id[A] = fa match {
      case Forward(p, length) => forward(p, length)
      case Backward(p, length) => backward(p, length)
      case RotateLeft(p) => rotateLeft(p)
      case RotateRight(p) => rotateRight(p)
      case ShowPosition(p) => println(s"showing position $p")
    }
  }

  type LogoApp[A] = Coproduct[Instruction, PencilInstruction, A]
  val interpreter: LogoApp ~> Id = InterpreterId or PenInterpreterId


  def program(start: Position)(implicit M: dsl.Moves[LogoApp], A: dsl.PencilActions[LogoApp]): Free[LogoApp, Unit] = {
    import A._
    import M._
    for {
      p1 <- forward(start, 10)
      p2 <- right(p1)
      _ <- pencilDown(p2)
      p3 <- forward(p2, 10)
      _ <- pencilUp(p3)
      _ <- showPosition(p3)
    } yield ()
  }

}

object LogoExample extends App {
  val program = Logo.program(Position(0.0, 0.0, Heading.Right()))

  program.foldMap(Logo.interpreter)
}






