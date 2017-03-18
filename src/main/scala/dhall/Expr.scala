package dhall

import cats.Monad
import cats.functor.Bifunctor
import dhall.utilities.TypeLevelFunctions.Partial2

sealed trait Expr[+S, +A] {
  import Expr._

  def map[B](f: A => B): Expr[S, B] = flatMap(a => Embed(f(a)))
  def ap[SS >: S, B](f: Expr[SS, A => B]): Expr[SS, B] = f.flatMap(map)
  def leftMap[T](f: S => T): Expr[T, A] = this match {
    case constant: Const => constant
    case variable: Var => variable
    case Lam(label, typ, body) => Lam(label, typ.leftMap(f), body.leftMap(f))
    case Quant(label, typ, body) => Quant(label, typ.leftMap(f), body.leftMap(f))
    case App(function, value) => App(function.leftMap(f), value.leftMap(f))
    case Let(label, typ, expr, body) => Let(label, typ.map(_.leftMap(f)), expr.leftMap(f), body.leftMap(f))
    case Annot(e1, e2) => Annot(e1.leftMap(f), e2.leftMap(f))
    case BoolType => BoolType
    case boolLit: BoolLit => boolLit
    case BoolAnd(e1, e2) => BoolAnd(e1.leftMap(f), e2.leftMap(f))
    case BoolOr(e1, e2) => BoolOr(e1.leftMap(f), e2.leftMap(f))
    case BoolEQ(e1, e2) => BoolEQ(e1.leftMap(f), e2.leftMap(f))
    case BoolNE(e1, e2) => BoolNE(e1.leftMap(f), e2.leftMap(f))
    case BoolIf(ifPart, thenPart, elsePart) => BoolIf(ifPart.leftMap(f), thenPart.leftMap(f), elsePart.leftMap(f))
    case NaturalType => NaturalType
    case naturalLit: NaturalLit => naturalLit
    case NaturalFold => NaturalFold
    case NaturalBuild => NaturalBuild
    case NaturalIsZero => NaturalIsZero
    case NaturalEven => NaturalEven
    case NaturalOdd => NaturalOdd
    case NaturalPlus(e1, e2) => NaturalPlus(e1.leftMap(f), e2.leftMap(f))
    case NaturalTimes(e1, e2) => NaturalTimes(e1.leftMap(f), e2.leftMap(f))
    case IntegerType => IntegerType
    case integerLit: IntegerLit => integerLit
    case DoubleType => DoubleType
    case doubleLit: DoubleLit => doubleLit
    case StringType => StringType
    case stringLit: StringLit => stringLit
    case StringAppend(e1, e2) => StringAppend(e1.leftMap(f), e2.leftMap(f))
    case ListType => ListType
    case ListLit(typeParam, value) => ListLit(typeParam.map(_.leftMap(f)), value.map(_.leftMap(f)))
    case ListBuild => ListBuild
    case ListFold => ListFold
    case ListLength => ListLength
    case ListHead => ListHead
    case ListLast => ListLast
    case ListIndexed => ListIndexed
    case ListReverse => ListReverse
    case OptionalType => OptionalType
    case OptionalLit(typeParam, value) => OptionalLit(typeParam.leftMap(f), value.map(_.leftMap(f)))
    case OptionalFold => OptionalFold
    case Record(mapping) => Record(mapping map {case (k, v) => k -> v.leftMap(f)})
    case RecordLit(mapping) => RecordLit(mapping map {case (k, v) => k -> v.leftMap(f)})
    case Union(mapping) => Union(mapping map {case (k, v) => k -> v.leftMap(f)})
    case Combine(e1, e2) => Combine(e1.leftMap(f), e2.leftMap(f))
    case Merge(e1, e2, typ) => Merge(e1.leftMap(f), e2.leftMap(f), typ.leftMap(f))
    case Field(record, name) => Field(record.leftMap(f), name)
    case Note(tag, e) => Note(f(tag), e.leftMap(f))
    case Embed(a) => Embed(a)
  }

  def flatMap[SS >: S, B](f: A => Expr[SS, B]): Expr[SS, B] = this match {
    case constant: Const => constant
    case variable: Var => variable
    case Lam(label, typ, body) => Lam(label, typ.flatMap(f), body.flatMap(f))
    case Quant(label, typ, body) => Quant(label, typ.flatMap(f), body.flatMap(f))
    case App(function, value) => App(function.flatMap(f), value.flatMap(f))
    case Let(label, typ, expr, body) => Let(label, typ.map(_.flatMap(f)), expr.flatMap(f), body.flatMap(f))
    case Annot(e1, e2) => Annot(e1.flatMap(f), e2.flatMap(f))
    case BoolType => BoolType
    case boolLit: BoolLit => boolLit
    case BoolAnd(e1, e2) => BoolAnd(e1.flatMap(f), e2.flatMap(f))
    case BoolOr(e1, e2) => BoolOr(e1.flatMap(f), e2.flatMap(f))
    case BoolEQ(e1, e2) => BoolEQ(e1.flatMap(f), e2.flatMap(f))
    case BoolNE(e1, e2) => BoolNE(e1.flatMap(f), e2.flatMap(f))
    case BoolIf(ifPart, thenPart, elsePart) => BoolIf(ifPart.flatMap(f), thenPart.flatMap(f), elsePart.flatMap(f))
    case NaturalType => NaturalType
    case naturalLit: NaturalLit => naturalLit
    case NaturalFold => NaturalFold
    case NaturalBuild => NaturalBuild
    case NaturalIsZero => NaturalIsZero
    case NaturalEven => NaturalEven
    case NaturalOdd => NaturalOdd
    case NaturalPlus(e1, e2) => NaturalPlus(e1.flatMap(f), e2.flatMap(f))
    case NaturalTimes(e1, e2) => NaturalTimes(e1.flatMap(f), e2.flatMap(f))
    case IntegerType => IntegerType
    case integerLit: IntegerLit => integerLit
    case DoubleType => DoubleType
    case doubleLit: DoubleLit => doubleLit
    case StringType => StringType
    case stringLit: StringLit => stringLit
    case StringAppend(e1, e2) => StringAppend(e1.flatMap(f), e2.flatMap(f))
    case ListType => ListType
    case ListLit(typeParam, value) => ListLit(typeParam.map(_.flatMap(f)), value.map(_.flatMap(f)))
    case ListBuild => ListBuild
    case ListFold => ListFold
    case ListLength => ListLength
    case ListHead => ListHead
    case ListLast => ListLast
    case ListIndexed => ListIndexed
    case ListReverse => ListReverse
    case OptionalType => OptionalType
    case OptionalLit(typeParam, value) => OptionalLit(typeParam.flatMap(f), value.map(_.flatMap(f)))
    case OptionalFold => OptionalFold
    case Record(mapping) => Record(mapping map {case (k, v) => k -> v.flatMap(f)})
    case RecordLit(mapping) => RecordLit(mapping map {case (k, v) => k -> v.flatMap(f)})
    case Union(mapping) => Union(mapping map {case (k, v) => k -> v.flatMap(f)})
    case Combine(e1, e2) => Combine(e1.flatMap(f), e2.flatMap(f))
    case Merge(e1, e2, typ) => Merge(e1.flatMap(f), e2.flatMap(f), typ.flatMap(f))
    case Field(record, name) => Field(record.flatMap(f), name)
    case Note(tag, e) => Note(tag, e.flatMap(f))
    case Embed(a) => f(a)
  }
}

object Expr extends ExprInstances {
  sealed trait Const extends Expr[Nothing, Nothing]
  object Const {
   case object Type extends Const
   case object Kind extends Const
  }

  case class Var(label: String, index: Int) extends Expr[Nothing, Nothing]
  case class Lam[+S, +A](domainLabel: String, domain: Expr[S, A], body: Expr[S, A]) extends Expr[S, A]
  case class Quant[+S, +A](domainLabel: String, domain: Expr[S, A], codomain: Expr[S, A]) extends Expr[S, A]
  case class App[+S, +A](function: Expr[S, A], value: Expr[S, A]) extends Expr[S, A]
  case class Let[+S, +A](label: String, typ: Option[Expr[S, A]], expr: Expr[S, A], body: Expr[S, A]) extends Expr[S, A]
  case class Annot[+S, +A](e1: Expr[S, A], e2: Expr[S, A]) extends Expr[S, A]

  case object BoolType extends Expr[Nothing, Nothing]
  case class BoolLit(value: Boolean) extends Expr[Nothing, Nothing]
  case class BoolAnd[+S, +A](e1: Expr[S, A], e2: Expr[S, A]) extends Expr[S, A]
  case class BoolOr[+S, +A](e1: Expr[S, A], e2: Expr[S, A]) extends Expr[S, A]
  case class BoolEQ[+S, +A](e1: Expr[S, A], e2: Expr[S, A]) extends Expr[S, A]
  case class BoolNE[+S, +A](e1: Expr[S, A], e2: Expr[S, A]) extends Expr[S, A]
  case class BoolIf[+S, +A](ifPart: Expr[S, A], thenPart: Expr[S, A], elsePart: Expr[S, A]) extends Expr[S, A]

  case object NaturalType extends Expr[Nothing, Nothing]
  //TODO: use correct Natural type instead of Int
  case class NaturalLit(value: Int) extends Expr[Nothing, Nothing]
  object NaturalFold extends Expr[Nothing, Nothing]
  object NaturalBuild extends Expr[Nothing, Nothing]
  object NaturalIsZero extends Expr[Nothing, Nothing]
  object NaturalEven extends Expr[Nothing, Nothing]
  object NaturalOdd extends Expr[Nothing, Nothing]
  case class NaturalPlus[+S, +A](e1: Expr[S, A], e2: Expr[S, A]) extends Expr[S, A]
  case class NaturalTimes[+S, +A](e1: Expr[S, A], e2: Expr[S, A]) extends Expr[S, A]

  case object IntegerType extends Expr[Nothing, Nothing]
  case class IntegerLit(value: Int) extends Expr[Nothing, Nothing]

  object DoubleType extends Expr[Nothing, Nothing]
  case class DoubleLit(value: Double) extends Expr[Nothing, Nothing]

  object StringType extends Expr[Nothing, Nothing]
  case class StringLit(value: String) extends Expr[Nothing, Nothing]
  case class StringAppend[+S, +A](e1: Expr[S, A], e2: Expr[S, A]) extends Expr[S, A]

  object ListType extends Expr[Nothing, Nothing]
  case class ListLit[+S, +A](typeParam: Option[Expr[S, A]], value: Seq[Expr[S, A]]) extends Expr[S, A]
  case object ListBuild extends Expr[Nothing, Nothing]
  case object ListFold extends Expr[Nothing, Nothing]
  case object ListLength extends Expr[Nothing, Nothing]
  case object ListHead extends Expr[Nothing, Nothing]
  case object ListLast extends Expr[Nothing, Nothing]
  case object ListIndexed extends Expr[Nothing, Nothing]
  case object ListReverse extends Expr[Nothing, Nothing]

  case object OptionalType extends Expr[Nothing, Nothing]
  case class OptionalLit[+S, +A](typeParam: Expr[S, A], value: Seq[Expr[S, A]]) extends Expr[S, A]
  case object OptionalFold extends Expr[Nothing, Nothing]

  case class Record[+S, +A](mapping: Map[String, Expr[S, A]]) extends Expr[S, A]
  case class RecordLit[+S, +A](mapping: Map[String, Expr[S, A]]) extends Expr[S, A]

  case class Union[+S, +A](mapping: Map[String, Expr[S, A]]) extends Expr[S, A]
  case class UnionLit[+S, +A](t: String, e: Expr[S, A], m: Map[String, Expr[S, A]])

  case class Combine[+S, +A](e1: Expr[S, A], e2: Expr[S, A]) extends Expr[S, A]
  case class Merge[+S, +A](e1: Expr[S, A], e2: Expr[S, A], typ: Expr[S, A]) extends Expr[S, A]
  case class Field[+S, +A](record: Expr[S, A], name: String) extends Expr[S, A]
  case class Note[+S, +A](tag: S, e: Expr[S, A]) extends Expr[S, A]
  case class Embed[+A](value: A) extends Expr[Nothing, A]
}

private[dhall] sealed trait ExprInstances {
  import Expr.Embed

  implicit def exprMonad[S] = new Monad[Partial2[Expr, S]#Apply] {
    override def map[A, B](fa: Expr[S, A])(f: A => B): Expr[S, B] =
      fa.map(f)

    def pure[A](x: A): Expr[S, A] = Embed(x)

    def flatMap[A, B](fa: Expr[S, A])(f: A => Expr[S, B]): Expr[S, B] =
      fa.flatMap(f)

    // TODO: This is not correct, not tailrec.
    def tailRecM[A, B](a: A)(f: A => Expr[S, Either[A, B]]): Expr[S, B] =
      f(a).flatMap(_.fold(tailRecM(_)(f), pure))
  }

  implicit val exprBifunctor = new Bifunctor[Expr] {
    def bimap[S, A, T, B](expr: Expr[S, A])(f: S => T, g: A => B): Expr[T, B] =
      expr.map(g).leftMap(f)
  }
}
