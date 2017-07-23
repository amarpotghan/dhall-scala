package dhall

import dhall.Expr.{NaturalType, Var, _}
import scala.collection.immutable.Map

object PrettyPrinter {

  def prettyPrint[S, A](expr: Expr[S, A]): String =
    buildExpr(expr)

  def buildLabel(label: String): String =
    if (reservedIdentifiers.contains(label))
      "`" + label + "`"
    else label

  def buildNumber(n: Int): String = n.toString

  def buildNumber(n: Double): String = n.toString

  def buildNumber(n: Float): String = n.toString

  def buildExpr[S, A]: (Expr[S, A]) => String = buildExprA[S, A]


  def buildExprA[S, A](expr: Expr[S, A]): String = expr match {
    case Annot(a, b) => buildExprB(a) + " : " + buildExprA(b)
    case Note(_, b) => buildExprA(b)
    case _ => buildExprB(expr)
  }

  def buildExprB[S, A](expr: Expr[S, A]): String = expr match {
    case Lam(a, b, c) => "λ(" + buildLabel(a) + " : " + buildExprA(b) + ") → " + buildExprB(c)
    case Quant("_", b, c) => buildExprC(b) + " → " + buildExprB(c)
    case Quant(a, b, c) => "∀(" + buildLabel(a) + " : " + buildExprA(b) + ") → " + buildExprB(c)
    case BoolIf(a, b, c) => "if " + buildExprA(a) + " then " + buildExprB(b) + " else " + buildExprC(c)
    case Let(a, None, c, d) => "let " + buildLabel(a) + " = " + buildExprA(c) + " in " + buildExprB(d)
    case Let(a, Some(b), c, d) => "let " + buildLabel(a) + " : " + buildExprA(b) + " = " + buildExprA(c) + " in " + buildExprB(d)
    case ListLit(None, b) => "[" + buildElems(b.toList) + "]"
    case ListLit(Some(a), b) => "[" + buildElems(b.toList) + "] : List " + buildExprE(a)
    case OptionalLit(a, b) => "[" + buildElems(b.toList) + "] : Optional " + buildExprE(a)
    case Merge(a, b, c) => "merge " + buildExprE(a) + " " + buildExprE(b) + " : " + buildExprD(c)
    case Note(_, b) => buildExprB(b)
    case _ => buildExprC(expr)
  }

  def buildExprC[S, A]: (Expr[S, A]) => String = buildExprC0[S, A]

  def buildExprC0[S, A](expr: Expr[S, A]): String = expr match {
    case BoolOr(a, b) => buildExprC1(a) + " || " + buildExprC0(b)
    case Note(_, b) => buildExprC0(b)
    case _ => buildExprC1(expr)
  }

  def buildExprC1[S, A](expr: Expr[S, A]): String = expr match {
    case StringAppend(a, b) => buildExprC2(a) + " ++ " + buildExprC1(b)
    case Note(_, b) => buildExprC1(b)
    case _ => buildExprC2(expr)

  }

  def buildExprC2[S, A](expr: Expr[S, A]): String = expr match {
    case NaturalPlus(a, b) => buildExprC3(a) + " + " + buildExprC2(b)
    case Note(_, b) => buildExprC2(b)
    case _ => buildExprC3(expr)
  }

  def buildExprC3[S, A](expr: Expr[S, A]): String = expr match {
    case BoolAnd(a, b) => buildExprC4(a) + " && " + buildExprC3(b)
    case Note(_, b) => buildExprC3(b)
    case _ => buildExprC4(expr)
  }

  def buildExprC4[S, A](expr: Expr[S, A]): String = expr match {
    case Combine(a, b) => buildExprC5(a) + " ∧ " + buildExprC4(b)
    case Note(_, b) => buildExprC4(b)
    case _ => buildExprC5(expr)
  }

  def buildExprC5[S, A](expr: Expr[S, A]): String = expr match {
    case NaturalTimes(a, b) => buildExprC6(a) + " * " + buildExprC5(b)
    case Note(_, b) => buildExprC5(b)
    case _ => buildExprC6(expr)
  }

  def buildExprC6[S, A](expr: Expr[S, A]): String = expr match {
    case BoolEQ(a, b) => buildExprC7(a) + " == " + buildExprC6(b)
    case Note(_, b) => buildExprC6(b)
    case _ => buildExprC7(expr)
  }

  def buildExprC7[S, A](expr: Expr[S, A]): String = expr match {
    case BoolNE(a, b) => buildExprD(a) + " != " + buildExprC7(b)
    case Note(_, b) => buildExprC7(b)
    case _ => buildExprD(expr)
  }

  def buildExprD[S, A](expr: Expr[S, A]): String = expr match {
    case App(a, b) => buildExprD(a) + " " + buildExprE(b)
    case Note(_, b) => buildExprD(b)
    case _ => buildExprE(expr)
  }

  def buildExprE[S, A](expr: Expr[S, A]): String = expr match {
    case Field(a, b) => buildExprE(a) + "." + buildLabel(b)
    case Note(_, b) => buildExprE(b)
    case _ => buildExprF(expr)
  }

  def buildExprF[S, A](expr: Expr[S, A]): String = expr match {
    case a: Var => buildVar(a)
    case Const.Type => "Type"
    case Const.Kind => "Kind"
    case BoolType => "Bool"
    case StringType => "String"
    case NaturalType => "Natural"
    case NaturalFold => "Natural/fold"
    case NaturalBuild => "Natural/build"
    case NaturalIsZero => "Natural/isZero"
    case NaturalEven => "Natural/even"
    case NaturalOdd => "Natural/odd"
    case IntegerType => "Integer"
    case DoubleType => "Double"
    case ListType => "List"
    case ListBuild => "List/build"
    case ListFold => "List/fold"
    case ListLength => "List/length"
    case ListHead => "List/head"
    case ListLast => "List/last"
    case ListIndexed => "List/indexed"
    case ListReverse => "List/reverse"
    case OptionalType => "Optional"
    case OptionalFold => "Optional/fold"
    case BoolLit(true) => "True"
    case BoolLit(false) => "False"
    case IntegerLit(a) => buildNumber(a)
    case NaturalLit(a) => "+" + a
    case DoubleLit(a) => buildNumber(a)
    case StringLit(a) => a
    case Record(a) => buildRecord(a)
    case RecordLit(a) => buildRecordLit(a)
    case Union(a) => buildUnion(a)
    case UnionLit(a, b, c) => buildUnionLit(a, b, c)
    case Embed(a) => a.toString
    case Note(_, b) => buildExprF(b)
    case _ => "(" + buildExprA(expr) + ")"
  }


  def buildVar(vr: Var): String = vr match {
    case Var(x, 0) => buildLabel(x)
    case Var(x, n) => buildLabel(x) + "@" + buildNumber(n)
  }

  def buildElems[S, A](elems: List[Expr[S, A]]): String = elems match {
    case List() => ""
    case List(a) => buildExprA(a)
    case (a :: as) => buildExprA(a) + ", " + buildElems(as)
  }

  def buildRecordLit[S, A](mapping: Map[String, Expr[S, A]]): String = mapping.toList match {
    case List() => "{=}"
    case list => "{" + buildFieldValues(list) + "}"
  }

  def buildFieldValues[S, A](fieldValues: List[(String, Expr[S, A])]): String = fieldValues match {
    case List() => ""
    case List(a) => buildFieldValue(a)
    case (a :: bs) => buildFieldValue(a) + ", " + buildFieldValues(bs)
  }

  def buildFieldValue[S, A](fieldValue: (String, Expr[S, A])): String =
    buildLabel(fieldValue._1) + " = " + buildExprA(fieldValue._2)


  def buildRecord[S, A](mapping: Map[String, Expr[S, A]]): String = mapping.toList match {
    case List() => "{}"
    case list => "{ " + buildFieldTypes(list) + " }"
  }

  def buildFieldTypes[S, A](fieldTypes: List[(String, Expr[S, A])]): String = fieldTypes match {
    case List() => ""
    case List(a) => buildFieldType(a)
    case (a :: bs) => buildFieldType(a) + ", " + buildFieldTypes(bs)
  }

  def buildFieldType[S, A](fieldType: (String, Expr[S, A])): String =
    buildLabel(fieldType._1) + " : " + buildExprA(fieldType._2)

  def buildUnion[S, A](mapping: Map[String, Expr[S, A]]): String = mapping.toList match {
    case List() => "<>"
    case list => "< " + buildAlternativeTypes(list) + " >"
  }

  def buildAlternativeTypes[S, A](altTypes: List[(String, Expr[S, A])]): String = altTypes match {
    case List() => ""
    case List(a) => buildAlternativeType(a)
    case (a :: bs) => buildAlternativeType(a) + " | " + buildAlternativeTypes(bs)
  }

  def buildAlternativeType[S, A](altType: (String, Expr[S, A])): String =
    buildLabel(altType._1) + " : " + buildExprA(altType._2)

  def buildUnionLit[S, A](t: String, e: Expr[S, A], mapping: Map[String, Expr[S, A]]): String = mapping.toList match {
    case List() => "< " +
      buildLabel(t) +
      " = " +
      buildExprA(e) +
      " >"
    case list => "< " +
      buildLabel(t) +
      " = " +
      buildExprA(e) +
      " | " +
      buildAlternativeTypes(list) +
      " >"
  }

  val reservedIdentifiers =
    Set(
      "let"
      , "in"
      , "Type"
      , "Kind"
      , "forall"
      , "Bool"
      , "True"
      , "False"
      , "merge"
      , "if"
      , "then"
      , "else"
      , "as"
      , "using"
      , "Natural"
      , "Natural/fold"
      , "Natural/build"
      , "Natural/isZero"
      , "Natural/even"
      , "Natural/odd"
      , "Natural/toInteger"
      , "Natural/show"
      , "Integer"
      , "Integer/show"
      , "Double"
      , "Double/show"
      , "Text"
      , "List"
      , "List/build"
      , "List/fold"
      , "List/length"
      , "List/head"
      , "List/last"
      , "List/indexed"
      , "List/reverse"
      , "Optional"
      , "Optional/fold"
    )


}
