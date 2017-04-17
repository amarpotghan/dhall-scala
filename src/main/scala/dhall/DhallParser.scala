package dhall

import dhall.Expression.{Embed, Lambda, Let, ListLit, Pi, Union, UnionLit}
import org.parboiled2._

import scala.util.Try

class DhallParser private[dhall](val input: ParserInput) extends Parser {

  def InputLine: Rule1[Expression[Nothing, Path]] = rule {
    ExpressionRule ~ EOI
  }

  def ExpressionRule: Rule1[Expression[Nothing, Path]] = rule {
    EnvExpression |
    UrlExpression |
    FileExpression |
    ListLiteralExpression |
    LambdaExpression |
    QuantExpression |
    LetExpression |
    UnionExpression |
    UnionLiteralExpression
  }

  def EnvExpression: Rule1[Embed[Env]] = rule {
    ws("env") ~ ws(":") ~ capture(oneOrMore(Identifier)) ~> ((path: String) => Embed(Env(path)))
  }

  def UrlExpression: Rule1[Embed[Url]] = rule {
    capture((ws("http://") | ws("https://")) ~ oneOrMore(VisibleChar)) ~> ((path: String) => Embed(Url(path)))
  }

  def FileExpression: Rule1[Embed[File]] = rule {
    capture((ws("/") | ws("./") | ws("../")) ~ oneOrMore(VisibleChar)) ~> ((path: String) => Embed(File(path)))
  }

  def LambdaExpression: Rule1[Lambda[Nothing, Path]] = rule {
    (LambdaSymbol ~ ws("(") ~ capture(oneOrMore(Identifier)) ~ ws(":") ~ ExpressionRule ~ ws(")") ~ ArrowSymbol ~ ExpressionRule) ~>
      ((label: String, domain: Expression[Nothing, Path], body: Expression[Nothing, Path]) => Lambda(label, domain, body))
  }

  def QuantExpression: Rule1[Pi[Nothing, Path]] = rule {
    (QuantSymbol ~ ws("(") ~ capture(oneOrMore(Identifier)) ~ ws(":") ~ ExpressionRule ~ ws(")") ~ ArrowSymbol ~ ExpressionRule) ~>
      ((label: String, domain: Expression[Nothing, Path], codomain: Expression[Nothing, Path]) => Pi(label, domain, codomain))
  }

  def LetExpression: Rule1[Let[Nothing, Path]] = rule {
    (ws("let") ~ capture(oneOrMore(Identifier)) ~ optional(ws(":") ~ ExpressionRule) ~ ws("=") ~ ExpressionRule ~ ws("in") ~ ExpressionRule) ~>
      ((label: String, typ: Option[Expression[Nothing, Path]], expr: Expression[Nothing, Path], body: Expression[Nothing, Path]) =>
        Let(label, typ, expr, body))
  }

  def ListLiteralExpression: Rule1[ListLit[Nothing, Path]] = rule {
    (ws("[") ~ zeroOrMore(ExpressionRule).separatedBy(",") ~ ws("]")) ~> ((xs: Seq[Expression[Nothing, Path]]) => ListLit(None, xs))
  }

  def AlternativeType: Rule1[(String, Expression[Nothing, Path])] = rule {
    (capture(oneOrMore(Identifier)) ~ ws(":") ~ ExpressionRule) ~> ((k: String, v: Expression[Nothing, Path]) => (k, v))
  }

  def AlternativeTypes: Rule1[Map[String, Expression[Nothing, Path]]] = rule {
    zeroOrMore(AlternativeType).separatedBy(ws("|")) ~> ((xs: Seq[(String, Expression[Nothing, Path])]) => xs.toMap)
  }

  def UnionExpression: Rule1[Union[Nothing, Path]] = rule {
    (ws("<") ~ AlternativeTypes ~ ws(">")) ~> (Union(_: Map[String, Expression[Nothing, Path]]))
  }

  def UnionLiteralExpression: Rule1[UnionLit[Nothing, Path]] = rule {
    (ws("<") ~ capture(oneOrMore(Identifier)) ~ ws("=") ~ ExpressionRule ~ optional(ws("|") ~ AlternativeTypes) ~ ws(">")) ~>
      ((label: String, expr: Expression[Nothing, Path], map: Option[Map[String, Expression[Nothing, Path]]]) =>
        UnionLit(label, expr, map.getOrElse(Map.empty)))
  }

  def Identifier: Rule0 = rule {
    CharPredicate.Alpha
  }

  def VisibleChar: Rule0 = rule {
    CharPredicate.Visible
  }

  def ArrowSymbol: Rule0 = rule {
    ws("->") | ws("→")
  }

  def LambdaSymbol: Rule0 = rule {
    ws("\\") | ws("λ")
  }

  def QuantSymbol: Rule0 = rule {
    ws("forall") | ws("∀")
  }

  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")

  def ws(toSurround: String): Rule0 = rule { zeroOrMore(WhiteSpaceChar) ~ toSurround ~ zeroOrMore(WhiteSpaceChar) }
}

object DhallParser {
  def parse(input: String): Try[Expression[Nothing, Path]] = {
    new DhallParser(input).InputLine.run()
  }
}
