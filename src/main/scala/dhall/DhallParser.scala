package dhall

import dhall.Expr.Embed
import org.parboiled2._

import scala.util.Try

class DhallParser private[dhall](val input: ParserInput) extends Parser {

  def InputLine: Rule1[Embed[String]] = rule {
    Expression ~ EOI
  }

  def Expression: Rule1[Embed[String]] = rule {
    Environment
  }

  def Environment: Rule1[Embed[String]] = rule {
    "env:" ~ capture(oneOrMore(Identifier)) ~> (Expr.Embed(_: String))
  }

  def Identifier: Rule0 = rule {
    CharPredicate.Alpha
  }
}

object DhallParser {
  def parse(input: String): Try[Embed[String]] = {
    new DhallParser(input).InputLine.run()
  }
}
