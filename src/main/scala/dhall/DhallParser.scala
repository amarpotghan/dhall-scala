package dhall

import dhall.Expr.Embed
import org.parboiled2._

import scala.util.Try

class DhallParser private[dhall](val input: ParserInput) extends Parser {

  def InputLine: Rule1[Embed[Env]] = rule {
    Expression ~ EOI
  }

  def Expression: Rule1[Embed[Env]] = rule {
    Environment
  }

  def Environment: Rule1[Embed[Env]] = rule {
    "env:" ~ capture(oneOrMore(Identifier)) ~> ((path: String) => Embed(Env(path)))
  }

  def Identifier: Rule0 = rule {
    CharPredicate.Alpha
  }
}

object DhallParser {
  def parse(input: String): Try[Embed[Env]] = {
    new DhallParser(input).InputLine.run()
  }
}
