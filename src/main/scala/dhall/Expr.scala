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
    case UnionLit(label, expr, mapping) => UnionLit(label, expr.leftMap(f), mapping map {case (k, v) => k -> v.leftMap(f)})
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
    case UnionLit(label, expr, mapping) => UnionLit(label, expr.flatMap(f), mapping map {case (k, v) => k -> v.flatMap(f)})
    case Combine(e1, e2) => Combine(e1.flatMap(f), e2.flatMap(f))
    case Merge(e1, e2, typ) => Merge(e1.flatMap(f), e2.flatMap(f), typ.flatMap(f))
    case Field(record, name) => Field(record.flatMap(f), name)
    case Note(tag, e) => Note(tag, e.flatMap(f))
    case Embed(a) => f(a)
  }

  def shiftVariableIndices[T](by: Int, variable: Var): Expr[T, A] = {
    def shift[X, Y, Z]: Expr[X, Z] => Expr[Y, Z] = _.shiftVariableIndices(by, variable)
    def withAdjustedIndex(label: String, expr: Expr[S, A]): Expr[T, A] = {
      val adjustedIndex = if(variable.label == label) variable.index + 1 else variable.index
      expr.shiftVariableIndices(by, variable.copy(index = adjustedIndex))
    }
    this match {
      case constant: Const => constant
      case Var(name, currentIndex) => {
        val newIndex = if(name == variable.label && currentIndex <= variable.index) currentIndex + by else currentIndex
        Var(name, newIndex)
      }
      case Lam(domainName, typeExpr, bodyExpr) => Lam(domainName, shift(typeExpr), withAdjustedIndex(domainName, bodyExpr))
      case Quant(domainName, typeExpr, bodyExpr) => Quant(domainName, shift(typeExpr), withAdjustedIndex(domainName, bodyExpr))
      case App(function, value) => App(shift(function), shift(value))
      case Let(label, typExprOpt, expr, bodyExpr) => Let(label, typExprOpt.map(shift), shift(expr), withAdjustedIndex(label, bodyExpr))
      case Annot(value, typ) => Annot(shift(value), shift(typ))
      case BoolType => BoolType
      case boolLit: BoolLit => boolLit
      case BoolAnd(e1, e2) => BoolAnd(shift(e1), shift(e2))
      case BoolOr(e1, e2) => BoolOr(shift(e1), shift(e2))
      case BoolEQ(e1, e2) => BoolEQ(shift(e1), shift(e2))
      case BoolNE(e1, e2) => BoolNE(shift(e1), shift(e2))
      case BoolIf(e1, e2, e3) => BoolIf(shift(e1), shift(e2), shift(e3))
      case NaturalType => NaturalType
      case naturalLit: NaturalLit => naturalLit
      case NaturalFold => NaturalFold
      case NaturalBuild => NaturalBuild
      case NaturalIsZero => NaturalIsZero
      case NaturalEven => NaturalEven
      case NaturalOdd => NaturalOdd
      case NaturalPlus(e1, e2) => NaturalPlus(shift(e1), shift(e2))
      case NaturalTimes(e1, e2) => NaturalTimes(shift(e1), shift(e2))
      case IntegerType => IntegerType
      case integer: IntegerLit => integer
      case DoubleType => DoubleType
      case double: DoubleLit => double
      case StringType => StringType
      case string: StringLit => string
      case StringAppend(s1, s2) => StringAppend(shift(s1), shift(s2))
      case ListType => ListType
      case ListLit(typExprOpt, ls) => ListLit(typExprOpt.map(shift), ls.map(shift))
      case ListBuild => ListBuild
      case ListFold => ListFold
      case ListLength => ListLength
      case ListHead => ListHead
      case ListLast => ListLast
      case ListIndexed => ListIndexed
      case ListReverse => ListReverse
      case OptionalType => OptionalType
      case OptionalFold => OptionalFold
      case OptionalLit(typeParam, values) => OptionalLit(shift(typeParam), values.map(shift))
      case Record(mapping) => Record(mapping map {case (k, v) => k -> shift(v)})
      case RecordLit(mapping) => RecordLit(mapping map {case (k, v) => k -> shift(v)})
      case Union(mapping) => Union(mapping map {case (k, v) => k -> shift(v)})
      case UnionLit(label, expr, mapping) => UnionLit(label, shift(expr), mapping map {case (k, v) => k -> shift(expr)})
      case Combine(e1, e2) => Combine(shift(e1), shift(e2))
      case Merge(e1, e2, typ) => Merge(shift(e1), shift(e2), shift(typ))
      case Field(record, name) => Field(shift(record), name)
      case Note(_, e) => shift(e)
      case Embed(embedded) => Embed(embedded)
    }
  }

  def substitute[T, AA >: A](variable: Var, by: Expr[T, AA]): Expr[T, AA] = {
    def subst: Expr[S, AA] => Expr[T, AA] = _.substitute(variable, by)
    def withShift(label: String, expr: Expr[S, AA]): Expr[T, AA] = {
      val shifted = if(variable.label == label) variable.index + 1 else variable.index
      expr.substitute(variable.copy(index = shifted), by.shiftVariableIndices(1, Var(label, 0)))
    }

    this match {
      case const: Const => const
      case Lam(label, typExpr, bodyExpr) => Lam(label, subst(typExpr), withShift(label, bodyExpr))
      case Quant(label, typeExpr, bodyExpr) => Quant(label, subst(typeExpr), withShift(label, bodyExpr))
      case App(function, value) => App(subst(function), subst(value))
      case Let(label, typExprOpt, valueExpr, bodyExpr) => Let(label, typExprOpt.map(subst), subst(valueExpr), withShift(label, bodyExpr))
      case v: Var => if(v == variable) by else v
      case Annot(e1, e2) => Annot(subst(e1), subst(e2))
      case BoolType => BoolType
      case boolLit: BoolLit => boolLit
      case BoolAnd(e1, e2) => BoolAnd(subst(e1), subst(e2))
      case BoolOr(e1, e2) => BoolOr(subst(e1), subst(e2))
      case BoolEQ(e1, e2) => BoolEQ(subst(e1), subst(e2))
      case BoolNE(e1, e2) => BoolNE(subst(e1), subst(e2))
      case BoolIf(ifPart, thenPart, elsePart) => BoolIf(subst(ifPart), subst(thenPart), subst(elsePart))
      case NaturalType => NaturalType
      case NaturalLit(n) => NaturalLit(n)
      case NaturalFold => NaturalFold
      case NaturalBuild => NaturalBuild
      case NaturalIsZero => NaturalIsZero
      case NaturalEven => NaturalEven
      case NaturalOdd => NaturalOdd
      case NaturalPlus(e1, e2) => NaturalPlus(subst(e1), subst(e2))
      case NaturalTimes(e1, e2) => NaturalTimes(subst(e1), subst(e2))
      case IntegerType => IntegerType
      case integer: IntegerLit => integer
      case DoubleType => DoubleType
      case double: DoubleLit => double
      case StringType => StringType
      case string: StringLit => string
      case StringAppend(s1, s2) => StringAppend(subst(s1), subst(s2))
      case ListType => ListType
      case ListLit(typ, values) => ListLit(typ.map(subst), values.map(subst))
      case ListBuild => ListBuild
      case ListFold => ListFold
      case ListLength => ListLength
      case ListHead => ListHead
      case ListLast => ListLast
      case ListReverse => ListReverse
      case ListIndexed => ListIndexed
      case OptionalType => OptionalType
      case OptionalLit(typ, values) => OptionalLit(subst(typ), values.map(subst))
      case OptionalFold => OptionalFold
      case Record(mapping) => Record(mapping map {case (k, v) => k -> subst(v)})
      case RecordLit(mapping) => RecordLit(mapping map {case (k, v) => k -> subst(v)})
      case Union(mapping) => Union(mapping map {case (k, v) => k -> subst(v)})
      case UnionLit(label, expr, mapping) => UnionLit(label, subst(expr), mapping map {case (k, v) => k -> subst(v)})
      case Combine(e1, e2) => Combine(subst(e1), subst(e2))
      case Merge(e1, e2, e3) => Merge(subst(e1), subst(e2), subst(e3))
      case Field(record, name) => Field(subst(record), name)
      case Note(_, expr) => subst(expr)
      case Embed(value) => Embed(value)
    }
  }

  def normalize[T]: Expr[T, A] = {
    this match {
      case const: Const => const
      case variable: Var => variable
      case Lam(label, typExpr, bodyExpr) => Lam(label, typExpr.normalize, bodyExpr.normalize)
      case Quant(label, domain, codomain) => Quant(label, domain.normalize, codomain.normalize)
      case App(function, value) => App(function.normalize, value.normalize)
      case Let(binding, _, expr, body) => body.substitute(Var(binding, 0), expr.shiftVariableIndices(1, Var(binding, 0))).shiftVariableIndices(-1, Var(binding, 0))
      case Annot(e1, _) => e1.normalize
      case BoolType => BoolType
      case BoolLit(b) => BoolLit(b)
      case BoolAnd(e1, e2) => (e1.normalize, e2.normalize) match {
        case (BoolLit(l1), BoolLit(l2)) => BoolLit(l1 && l2)
        case (x, y) => BoolAnd(x, y)
      }
      case BoolOr(e1, e2) => (e1.normalize, e2.normalize) match {
        case (BoolLit(l1), BoolLit(l2)) => BoolLit(l1 || l2)
        case (x, y) => BoolOr(x, y)
      }
      case BoolEQ(e1, e2) => (e1.normalize, e2.normalize) match {
        case (BoolLit(l1), BoolLit(l2)) => BoolLit(l1 == l2)
        case (x, y) => BoolEQ(x, y)
      }
      case BoolNE(e1, e2) => (e1.normalize, e2.normalize) match {
        case (BoolLit(l1), BoolLit(l2)) => BoolLit(l1 != l2)
        case (x, y) => BoolNE(x, y)
      }
      case BoolIf(ifExpr, thenExpr, elseExpr) => ifExpr.normalize match {
        case BoolLit(l) => if(l) thenExpr.normalize else elseExpr.normalize
        case other => BoolIf(other, thenExpr.normalize, elseExpr.normalize)
      }
      case NaturalType => NaturalType
      case NaturalLit(x) => NaturalLit(x)
      case NaturalFold => NaturalFold
      case NaturalBuild => NaturalBuild
      case NaturalIsZero => NaturalIsZero
      case NaturalEven => NaturalEven
      case NaturalOdd => NaturalOdd
      case NaturalPlus(e1, e2) => (e1.normalize, e2.normalize) match {
        case (NaturalLit(n1), NaturalLit(n2)) => NaturalLit(n1 + n2)
        case (x, y) => NaturalPlus(x, y)
      }
      case NaturalTimes(e1, e2) => (e1.normalize, e2.normalize) match {
        case (NaturalLit(n1), NaturalLit(n2)) => NaturalLit(n1 * n2)
        case (x, y) => NaturalTimes(x, y)
      }
      case IntegerType => IntegerType
      case IntegerLit(n) => IntegerLit(n)
      case DoubleType => DoubleType
      case DoubleLit(n) => DoubleLit(n)
      case StringType => StringType
      case StringLit(s) => StringLit(s)
      case StringAppend(e1, e2) => (e1.normalize, e2.normalize) match {
        case (StringLit(s1), StringLit(s2)) => StringLit(s1 ++ s2)
        case (x, y) => StringAppend(x, y)
      }
      case ListType => ListType
      case ListLit(typ, ls) => ListLit(typ.map(_.normalize), ls.map(_.normalize))
      case ListBuild => ListBuild
      case ListFold => ListFold
      case ListLength => ListLength
      case ListHead => ListHead
      case ListLast => ListLast
      case ListIndexed => ListIndexed
      case ListReverse => ListReverse
      case OptionalType => OptionalType
      case OptionalLit(t, es) => OptionalLit(t.normalize, es.map(_.normalize))
      case OptionalFold => OptionalFold
      case Record(mapping) => Record(mapping.map {case (k, v) => k -> v.normalize})
      case RecordLit(mapping) => RecordLit(mapping.map {case (k, v) => k -> v.normalize})
      case Union(mapping) => Union(mapping.map {case (k, v) => k -> v.normalize})
      case UnionLit(label, e, mapping) => UnionLit(label, e.normalize, mapping.map {case (k, v) => k -> v.normalize})
      case Combine(e1, e2) => {
        // TODO: extract util function
        def combineMaps(first: Map[String, Expr[S, A]], second: Map[String, Expr[S, A]]): Map[String, Expr[T, A]] = {
          val commonKeys = first.keySet intersect second.keySet
          val combinedKeyValues = commonKeys.map(k => k -> combine(first(k), second(k))).toMap
          val otherThanCommon = first.filterKeys(!commonKeys.contains(_)) ++ second.filterKeys(!commonKeys.contains(_))
          //test
          combinedKeyValues ++ (otherThanCommon.map {case (k, v) => k -> v.normalize})
        }
        def combine(e1: Expr[S, A], e2: Expr[S, A]): Expr[T, A] = (e1.normalize, e2.normalize) match {
          case (RecordLit(mapping1), RecordLit(mapping2)) => RecordLit(combineMaps(mapping1, mapping2).map {case (k, v) => k -> v.normalize})
          case (x, y) => Combine(x, y)
        }

        combine(e1, e2)
      }
      case Merge(e1, e2, e3) => {
        val e1Normalized = e1.normalize
        val e2Normalized = e2.normalize
        (e1Normalized, e2Normalized) match {
          case (RecordLit(mapping), UnionLit(ks, vs, _)) => {
            mapping.get(ks).fold[Expr[T, A]](Merge(e1Normalized, e2Normalized, e3.normalize))(_.normalize)
          }
          case (x, y) => Merge(x, y, e3.normalize)
        }
      }
      case Field(record, name) => record.normalize match {
        case RecordLit(fields) => fields.get(name).fold[Expr[T, A]](Field(RecordLit(fields.map {case (k, v) => k -> v.normalize}), name))(_.normalize)
        case other => Field(other, name)
      }
      case Note(_, expr) => expr.normalize
      case Embed(a) => Embed(a)
    }
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
  case object NaturalFold extends Expr[Nothing, Nothing]
  case object NaturalBuild extends Expr[Nothing, Nothing]
  case object NaturalIsZero extends Expr[Nothing, Nothing]
  case object NaturalEven extends Expr[Nothing, Nothing]
  case object NaturalOdd extends Expr[Nothing, Nothing]
  case class NaturalPlus[+S, +A](e1: Expr[S, A], e2: Expr[S, A]) extends Expr[S, A]
  case class NaturalTimes[+S, +A](e1: Expr[S, A], e2: Expr[S, A]) extends Expr[S, A]

  case object IntegerType extends Expr[Nothing, Nothing]
  case class IntegerLit(value: Int) extends Expr[Nothing, Nothing]

  case object DoubleType extends Expr[Nothing, Nothing]
  case class DoubleLit(value: Double) extends Expr[Nothing, Nothing]

  case object StringType extends Expr[Nothing, Nothing]
  case class StringLit(value: String) extends Expr[Nothing, Nothing]
  case class StringAppend[+S, +A](e1: Expr[S, A], e2: Expr[S, A]) extends Expr[S, A]

  case object ListType extends Expr[Nothing, Nothing]
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
  case class UnionLit[+S, +A](t: String, e: Expr[S, A], m: Map[String, Expr[S, A]]) extends Expr[S, A]

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
