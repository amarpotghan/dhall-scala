package dhall

import dhall.Expr.Embed
import org.parboiled2._

import scala.util.Try

class DhallParser private[dhall](val input: ParserInput) extends Parser {

  def InputLine: Rule1[Embed[Path]] = rule {
    Expression ~ EOI
  }

  def Expression: Rule1[Embed[Path]] = rule {
    EnvExpression | UrlExpression | FileExpression
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

  def Identifier: Rule0 = rule {
    CharPredicate.Alpha
  }

  def VisibleChar: Rule0 = rule {
    CharPredicate.Visible
  }
}

object DhallParser {
  def parse(input: String): Try[Embed[Path]] = {
    new DhallParser(input).InputLine.run()
  }
}
