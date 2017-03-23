package dhall

import org.specs2.matcher.Matchers
import org.specs2.mutable.Specification

class DhallParserSpec extends Specification with Matchers {

  "Dhall parser" should {

    "parse environment variable expressions" in {
      val expression = "env:PATH"

      DhallParser.parse(expression).get must equalTo(Expr.Embed(Env("PATH")))
    }
  }
}
