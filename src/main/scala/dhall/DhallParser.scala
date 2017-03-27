package dhall

import dhall.Expr.{Embed, ListLit}
import org.parboiled2._

import scala.util.Try

class DhallParser private[dhall](val input: ParserInput) extends Parser {

  def InputLine: Rule1[Expr[Nothing, Path]] = rule {
    Expression ~ EOI
  }

  def Expression: Rule1[Expr[Nothing, Path]] = rule {
    EnvExpression | UrlExpression | FileExpression | ListLiteralExpression
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

  def ListLiteralExpression: Rule1[ListLit[Nothing, Path]] = rule {
    ("[" ~ zeroOrMore(Expression).separatedBy(",") ~ "]") ~> ((xs: Seq[Expr[Nothing, Path]]) => ListLit(None, xs))
  }

  def Identifier: Rule0 = rule {
    CharPredicate.Alpha
  }

  def VisibleChar: Rule0 = rule {
    CharPredicate.Visible
  }
}

object DhallParser {
  def parse(input: String): Try[Expr[Nothing, Path]] = {
    new DhallParser(input).InputLine.run()
  }
}
