package dhall

import cats.Monad
import cats.functor.Bifunctor
import scala.util.Try

import dhall.utilities.TypeLevelFunctions.Partial2
import dhall.utilities.MapFunctions.RichMap

sealed trait Expression[+S, +A] extends Product with Serializable {
  import Expression._
  def map[B](f: A => B): Expression[S, B] = flatMap(a => Embed(f(a)))
  def ap[SS >: S, B](f: Expression[SS, A => B]): Expression[SS, B] = f.flatMap(map)
  def leftMap[T](f: S => T): Expression[T, A] = this match {
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
    case Record(mapping) => Record(mapping.mapValue(_.leftMap(f)))
    case RecordLit(mapping) => RecordLit(mapping.mapValue(_.leftMap(f)))
    case Union(mapping) => Union(mapping.mapValue(_.leftMap(f)))
    case UnionLit(label, expr, mapping) => UnionLit(label, expr.leftMap(f), mapping.mapValue(_.leftMap(f)))
    case Combine(e1, e2) => Combine(e1.leftMap(f), e2.leftMap(f))
    case Merge(e1, e2, typ) => Merge(e1.leftMap(f), e2.leftMap(f), typ.leftMap(f))
    case Field(record, name) => Field(record.leftMap(f), name)
    case Note(tag, e) => Note(f(tag), e.leftMap(f))
    case Embed(a) => Embed(a)
  }

  def flatMap[SS >: S, B](f: A => Expression[SS, B]): Expression[SS, B] = this match {
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
    case Record(mapping) => Record(mapping.mapValue(_.flatMap(f)))
    case RecordLit(mapping) => RecordLit(mapping.mapValue(_.flatMap(f)))
    case Union(mapping) => Union(mapping.mapValue(_.flatMap(f)))
    case UnionLit(label, expr, mapping) => UnionLit(label, expr.flatMap(f), mapping.mapValue(_.flatMap(f)))
    case Combine(e1, e2) => Combine(e1.flatMap(f), e2.flatMap(f))
    case Merge(e1, e2, typ) => Merge(e1.flatMap(f), e2.flatMap(f), typ.flatMap(f))
    case Field(record, name) => Field(record.flatMap(f), name)
    case Note(tag, e) => Note(tag, e.flatMap(f))
    case Embed(a) => f(a)
  }

  // N.B. - This method is not stack safe
  def shiftVariableIndices[T](by: Int, variable: Var): Expression[T, A] = {
    def shift[X, Y, Z]: Expression[X, Z] => Expression[Y, Z] = _.shiftVariableIndices(by, variable)
    def withAdjustedIndex(label: String, expr: Expression[S, A]): Expression[T, A] = {
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
      case Record(mapping) => Record(mapping.mapValue(shift))
      case RecordLit(mapping) => RecordLit(mapping.mapValue(shift))
      case Union(mapping) => Union(mapping.mapValue(shift))
      case UnionLit(label, expr, mapping) => UnionLit(label, shift(expr), mapping.mapValue(shift))
      case Combine(e1, e2) => Combine(shift(e1), shift(e2))
      case Merge(e1, e2, typ) => Merge(shift(e1), shift(e2), shift(typ))
      case Field(record, name) => Field(shift(record), name)
      case Note(_, e) => shift(e)
      case Embed(embedded) => Embed(embedded)
    }
  }

  // N.B. - This method is not stack safe
  def substitute[T, AA >: A](variable: Var, by: Expression[T, AA]): Expression[T, AA] = {
    def subst: Expression[S, AA] => Expression[T, AA] = _.substitute(variable, by)
    def withShift(label: String, expr: Expression[S, AA]): Expression[T, AA] = {
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
      case Record(mapping) => Record(mapping.mapValue(subst))
      case RecordLit(mapping) => RecordLit(mapping.mapValue(subst))
      case Union(mapping) => Union(mapping.mapValue(subst))
      case UnionLit(label, expr, mapping) => UnionLit(label, subst(expr), mapping.mapValue(subst))
      case Combine(e1, e2) => Combine(subst(e1), subst(e2))
      case Merge(e1, e2, e3) => Merge(subst(e1), subst(e2), subst(e3))
      case Field(record, name) => Field(subst(record), name)
      case Note(_, expr) => subst(expr)
      case Embed(value) => Embed(value)
    }
  }

  // N.B. - This method is not stack safe
  def normalize[T]: Expression[T, A] = {
    this match {
      case const: Const => const
      case variable: Var => variable
      case Lam(label, typExpr, bodyExpr) => Lam(label, typExpr.normalize, bodyExpr.normalize)
      case Quant(label, domain, codomain) => Quant(label, domain.normalize, codomain.normalize)
      case App(function, arg) => {
        function.normalize match {
          // normalize ((\x -> f x) a) => normalize (f a)
          case Lam(label, typExpr, bodyExpr) => bodyExpr.substitute(Var(label, 0), arg.shiftVariableIndices(1, Var(label, 0))).shiftVariableIndices(-1, Var(label, 0)).normalize
          case normalizedFunction => (normalizedFunction, arg.normalize) match {
            // normalize (List/build t ((List/fold t) e)) = normalize e
            case (App(ListBuild, _), App(App(ListFold, _), argument)) => argument.normalize
            case (App(ListFold, _), App(App(ListBuild, _), argument)) => argument.normalize
            case (NaturalBuild, App(NaturalFold, argument)) => argument.normalize
            case (NaturalFold, App(NaturalBuild, argument)) => argument.normalize
            // Natural/fold 2 Natural (+15) 1 = (15 + (15 + 1)) = 31
            case (App(App(App(NaturalFold, (NaturalLit(n))), t), f), empty) => (1 to n).foldRight(empty)((_, acc) => App(f, acc)).normalize
            case (NaturalBuild, v) => {
              val withLabels = App(App(App(v, NaturalType), Var("Succ", 0)), Var("Zero", 0)).normalize
              def normalized(e: Expression[S, A]): Boolean = Try(result(0, e)).toOption.fold[Boolean](false)(_ => true)
              def result(n: Int, e: Expression[S, A]): Int = e match {
                case App(Var("Succ", _), next) => result(n + 1, next)
                case Var("Zero", _) => n
                case _ => throw CompilerBug.NormalizerBug(s"${this.toString}.normalize")
              }
              if(normalized(withLabels)) NaturalLit(result(0, withLabels)) else App(normalizedFunction, v)
            }
            case (NaturalIsZero, (NaturalLit(n))) => BoolLit(n == 0)
            case (NaturalEven, (NaturalLit(n))) => BoolLit(n % 2 == 0)
            case (NaturalOdd, (NaturalLit(n))) => BoolLit(n % 2 != 0)
            case (App(ListBuild, t), v) => {
              // We first label the church encoded list using "Cons" and "Nil" variables
              val withLabels = App(App(App(v, App(ListType, t)), Var("Cons", 0)), Var("Nil", 0)).normalize
              def normalized(e: Expression[S, A]): Boolean = Try(result(Nil, e)).toOption.fold[Boolean](false)(_ => true)
              // we fold over the labeled church encoded list and create a Scala List
              def result[Tag, V](acc: List[Expression[Tag, V]], e: Expression[Tag, V]): List[Expression[Tag, V]] = e match {
                case App(App(Var("Cons", _), h), next) => result(h :: acc, next)
                case Var("Nil", _) => acc
                case _ => throw CompilerBug.NormalizerBug(s"${this.toString}.normalize")
              }
              if(normalized(withLabels)) ListLit(Some(t), result(Nil, withLabels)) else App(normalizedFunction, v)
            }
            case (App(App(App(ListFold, t), ListLit(_, ls)), f), seed) => ls.foldRight(seed)((e, acc) => App(App(f, e), acc)).normalize
            case (App(ListLength, _), ListLit(_, ls)) => NaturalLit(ls.size)
            case (App(ListHead, t), ListLit(_, ls)) => OptionalLit(t, ls.headOption.toList).normalize
            case (App(ListLast, t), ListLit(_, ls)) => OptionalLit(t, ls.lastOption.toList).normalize
            case (App(ListIndexed, t), ListLit(_, ls)) => {
              val typ = Record(Map("index" -> NaturalType, "value" -> t))
              ListLit(Some(typ), ls.zipWithIndex.map{ case (v, i) => RecordLit(Map("index" -> NaturalLit(i), "value" -> v))}).normalize
            }
            case (App(ListReverse, t), ListLit(_, ls)) => ListLit(Some(t), ls.reverse).normalize
            case (App(App(App(App(OptionalFold, t), OptionalLit(_, ls)), _), some), none) => ls.headOption.fold(none)(App(some, _)).normalize
            case (n, v) => App(n, v)
          }
        }
      }
      case Let(binding, _, expr, body) => body.substitute(Var(binding, 0), expr.shiftVariableIndices(1, Var(binding, 0))).shiftVariableIndices(-1, Var(binding, 0)).normalize
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
      case Record(mapping) => Record(mapping.mapValue(_.normalize))
      case RecordLit(mapping) => RecordLit(mapping.mapValue(_.normalize))
      case Union(mapping) => Union(mapping.mapValue(_.normalize))
      case UnionLit(label, e, mapping) => UnionLit(label, e.normalize, mapping.mapValue(_.normalize))
      case Combine(e1, e2) => {
        def combine(e1: Expression[T, A], e2: Expression[T, A]): Expression[T, A] = (e1, e2) match {
          case (RecordLit(mapping1), RecordLit(mapping2)) => RecordLit(mapping1.unionWith(combine(_, _), mapping2).mapValue(_.normalize))
          case (x, y) => Combine(x, y)
        }
        combine(e1.normalize, e2.normalize)
      }
      case Merge(e1, e2, e3) => {
        val e1Normalized = e1.normalize
        val e2Normalized = e2.normalize
        (e1Normalized, e2Normalized) match {
          case (RecordLit(mapping), UnionLit(ks, vs, _)) => {
            mapping.get(ks).fold[Expression[T, A]](Merge(e1Normalized, e2Normalized, e3.normalize))(r => App(r, vs).normalize)
          }
          case (x, y) => Merge(x, y, e3.normalize)
        }
      }
      case Field(record, name) => record.normalize match {
        case RecordLit(fields) => fields.get(name).fold[Expression[T, A]](Field(RecordLit(fields.mapValue(_.normalize)), name))(_.normalize)
        case other => Field(other, name)
      }
      case Note(_, expr) => expr.normalize
      case Embed(a) => Embed(a)
    }
  }
}


object Expression extends ExpressionInstances {
  sealed trait Const extends Expression[Nothing, Nothing]
  object Const {
   case object Type extends Const
   case object Kind extends Const
  }

  case class Var(label: String, index: Int) extends Expression[Nothing, Nothing]
  case class Lam[+S, +A](domainLabel: String, domain: Expression[S, A], body: Expression[S, A]) extends Expression[S, A]
  case class Quant[+S, +A](domainLabel: String, domain: Expression[S, A], codomain: Expression[S, A]) extends Expression[S, A]
  case class App[+S, +A](function: Expression[S, A], value: Expression[S, A]) extends Expression[S, A]
  case class Let[+S, +A](label: String, typ: Option[Expression[S, A]], expr: Expression[S, A], body: Expression[S, A]) extends Expression[S, A]
  case class Annot[+S, +A](e1: Expression[S, A], e2: Expression[S, A]) extends Expression[S, A]

  case object BoolType extends Expression[Nothing, Nothing]
  case class BoolLit(value: Boolean) extends Expression[Nothing, Nothing]
  case class BoolAnd[+S, +A](e1: Expression[S, A], e2: Expression[S, A]) extends Expression[S, A]
  case class BoolOr[+S, +A](e1: Expression[S, A], e2: Expression[S, A]) extends Expression[S, A]
  case class BoolEQ[+S, +A](e1: Expression[S, A], e2: Expression[S, A]) extends Expression[S, A]
  case class BoolNE[+S, +A](e1: Expression[S, A], e2: Expression[S, A]) extends Expression[S, A]
  case class BoolIf[+S, +A](ifPart: Expression[S, A], thenPart: Expression[S, A], elsePart: Expression[S, A]) extends Expression[S, A]

  case object NaturalType extends Expression[Nothing, Nothing]
  //TODO: use correct Natural type instead of Int
  case class NaturalLit(value: Int) extends Expression[Nothing, Nothing]
  case object NaturalFold extends Expression[Nothing, Nothing]
  case object NaturalBuild extends Expression[Nothing, Nothing]
  case object NaturalIsZero extends Expression[Nothing, Nothing]
  case object NaturalEven extends Expression[Nothing, Nothing]
  case object NaturalOdd extends Expression[Nothing, Nothing]
  case class NaturalPlus[+S, +A](e1: Expression[S, A], e2: Expression[S, A]) extends Expression[S, A]
  case class NaturalTimes[+S, +A](e1: Expression[S, A], e2: Expression[S, A]) extends Expression[S, A]

  case object IntegerType extends Expression[Nothing, Nothing]
  case class IntegerLit(value: Int) extends Expression[Nothing, Nothing]

  case object DoubleType extends Expression[Nothing, Nothing]
  case class DoubleLit(value: Double) extends Expression[Nothing, Nothing]

  case object StringType extends Expression[Nothing, Nothing]
  case class StringLit(value: String) extends Expression[Nothing, Nothing]
  case class StringAppend[+S, +A](e1: Expression[S, A], e2: Expression[S, A]) extends Expression[S, A]

  case object ListType extends Expression[Nothing, Nothing]
  case class ListLit[+S, +A](typeParam: Option[Expression[S, A]], value: Seq[Expression[S, A]]) extends Expression[S, A]
  case object ListBuild extends Expression[Nothing, Nothing]
  case object ListFold extends Expression[Nothing, Nothing]
  case object ListLength extends Expression[Nothing, Nothing]
  case object ListHead extends Expression[Nothing, Nothing]
  case object ListLast extends Expression[Nothing, Nothing]
  case object ListIndexed extends Expression[Nothing, Nothing]
  case object ListReverse extends Expression[Nothing, Nothing]

  case object OptionalType extends Expression[Nothing, Nothing]
  case class OptionalLit[+S, +A](typeParam: Expression[S, A], value: Seq[Expression[S, A]]) extends Expression[S, A]
  case object OptionalFold extends Expression[Nothing, Nothing]

  case class Record[+S, +A](mapping: Map[String, Expression[S, A]]) extends Expression[S, A]
  case class RecordLit[+S, +A](mapping: Map[String, Expression[S, A]]) extends Expression[S, A]

  case class Union[+S, +A](mapping: Map[String, Expression[S, A]]) extends Expression[S, A]
  case class UnionLit[+S, +A](t: String, e: Expression[S, A], m: Map[String, Expression[S, A]]) extends Expression[S, A]

  case class Combine[+S, +A](e1: Expression[S, A], e2: Expression[S, A]) extends Expression[S, A]
  case class Merge[+S, +A](e1: Expression[S, A], e2: Expression[S, A], typ: Expression[S, A]) extends Expression[S, A]
  case class Field[+S, +A](record: Expression[S, A], name: String) extends Expression[S, A]
  case class Note[+S, +A](tag: S, e: Expression[S, A]) extends Expression[S, A]
  case class Embed[+A](value: A) extends Expression[Nothing, A]

  // TODO: Improve error message
  sealed abstract class CompilerBug(msg: String) extends RuntimeException(msg)
  object CompilerBug {
    case class NormalizerBug(msg: String) extends CompilerBug(s"Bug in compiler, this should never happen. Please report. Message: $msg")
  }
}

private[dhall] sealed trait ExpressionInstances {
  import Expression.Embed

  implicit def exprMonad[S] = new Monad[Partial2[Expression, S]#Apply] {
    override def map[A, B](fa: Expression[S, A])(f: A => B): Expression[S, B] =
      fa.map(f)

    def pure[A](x: A): Expression[S, A] = Embed(x)

    def flatMap[A, B](fa: Expression[S, A])(f: A => Expression[S, B]): Expression[S, B] =
      fa.flatMap(f)

    // TODO: This is not correct, not tailrec.
    def tailRecM[A, B](a: A)(f: A => Expression[S, Either[A, B]]): Expression[S, B] =
      f(a).flatMap(_.fold(tailRecM(_)(f), pure))
  }

  implicit val exprBifunctor = new Bifunctor[Expression] {
    def bimap[S, A, T, B](expr: Expression[S, A])(f: S => T, g: A => B): Expression[T, B] =
      expr.map(g).leftMap(f)
  }
}
