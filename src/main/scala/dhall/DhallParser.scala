package dhall

import dhall.Expr.{Embed, Lam, ListLit, Quant, Union, UnionLit}
import org.parboiled2._

import scala.util.Try

class DhallParser private[dhall](val input: ParserInput) extends Parser {

  def InputLine: Rule1[Expr[Nothing, Path]] = rule {
    Expression ~ EOI
  }

  def Expression: Rule1[Expr[Nothing, Path]] = rule {
    EnvExpression |
    UrlExpression |
    FileExpression |
    ListLiteralExpression |
    LambdaExpression |
    QuantExpression |
    UnionExpression |
    UnionLiteralExpression
  }

  def EnvExpression: Rule1[Embed[Env]] = rule {
    "env:" ~ capture(oneOrMore(Identifier)) ~> ((path: String) => Embed(Env(path)))
  }

  def UrlExpression: Rule1[Embed[Url]] = rule {
    capture(("http://" | "https://") ~ oneOrMore(VisibleChar)) ~> ((path: String) => Embed(Url(path)))
  }

  def FileExpression: Rule1[Embed[File]] = rule {
    capture(("/" | "./" | "../") ~ oneOrMore(VisibleChar)) ~> ((path: String) => Embed(File(path)))
  }

  def LambdaExpression: Rule1[Lam[Nothing, Path]] = rule {
    (LambdaSymbol ~ "(" ~ capture(oneOrMore(Identifier)) ~ ":" ~ Expression ~ ")" ~ ArrowSymbol ~ Expression) ~>
      ((label: String, domain: Expr[Nothing, Path], body: Expr[Nothing, Path]) => Lam(label, domain, body))
  }

  def QuantExpression: Rule1[Quant[Nothing, Path]] = rule {
    (QuantSymbol ~ "(" ~ capture(oneOrMore(Identifier)) ~ ":" ~ Expression ~ ")" ~ ArrowSymbol ~ Expression) ~>
      ((label: String, domain: Expr[Nothing, Path], codomain: Expr[Nothing, Path]) => Quant(label, domain, codomain))
  }

  def ListLiteralExpression: Rule1[ListLit[Nothing, Path]] = rule {
    ("[" ~ zeroOrMore(Expression).separatedBy(",") ~ "]") ~> ((xs: Seq[Expr[Nothing, Path]]) => ListLit(None, xs))
  }

  def AlternativeType: Rule1[(String, Expr[Nothing, Path])] = rule {
    (capture(oneOrMore(Identifier)) ~ ":" ~ Expression) ~> ((k: String, v: Expr[Nothing, Path]) => (k, v))
  }

  def AlternativeTypes: Rule1[Map[String, Expr[Nothing, Path]]] = rule {
    zeroOrMore(AlternativeType).separatedBy("|") ~> ((xs: Seq[(String, Expr[Nothing, Path])]) => xs.toMap)
  }

  def UnionExpression: Rule1[Union[Nothing, Path]] = rule {
    ("<" ~ AlternativeTypes ~ ">") ~> (Union(_: Map[String, Expr[Nothing, Path]]))
  }

  def UnionLiteralExpression: Rule1[UnionLit[Nothing, Path]] = rule {
    ("<" ~ capture(oneOrMore(Identifier)) ~ "=" ~ Expression ~ optional("|" ~ AlternativeTypes) ~ ">") ~>
      ((label: String, expr: Expr[Nothing, Path], map: Option[Map[String, Expr[Nothing, Path]]]) =>
        UnionLit(label, expr, map.getOrElse(Map.empty)))
  }

  def Identifier: Rule0 = rule {
    CharPredicate.Alpha
  }

  def VisibleChar: Rule0 = rule {
    CharPredicate.Visible
  }

  def ArrowSymbol: Rule0 = rule {
    "->" | "→"
  }

  def LambdaSymbol: Rule0 = rule {
    "\\" | "λ"
  }

  def QuantSymbol: Rule0 = rule {
    "forall" | "∀"
  }
}

object DhallParser {
  def parse(input: String): Try[Expr[Nothing, Path]] = {
    new DhallParser(input).InputLine.run()
  }
}
