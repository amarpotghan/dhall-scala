package dhall

import org.specs2.matcher.Matchers
import org.specs2.mutable.Specification

class DhallParserSpec extends Specification with Matchers {

  "Dhall parser" should {

    "parse environment variable expressions" in {
      val expression = "env:PATH"

      DhallParser.parse(expression).get must equalTo(Expr.Embed(Env("PATH")))
    }

    "parse url expressions" should {
      "http url" in {
        val expression = "http://www.website.com:8080/path/to/resource?a=b&x=y#L726"

        DhallParser.parse(expression).get must equalTo(Expr.Embed(Url("http://www.website.com:8080/path/to/resource?a=b&x=y#L726")))
      }

      "https url" in {
        val expression = "https://www.website.com:8080/path/to/resource?a=b&x=y#L726"

        DhallParser.parse(expression).get must equalTo(Expr.Embed(Url("https://www.website.com:8080/path/to/resource?a=b&x=y#L726")))
      }
    }

    "parse file expressions" should {
      "starting with /" in {
        val expression = "/someFilePath"

        DhallParser.parse(expression).get must equalTo(Expr.Embed(File("/someFilePath")))
      }

      "starting with ./" in {
        val expression = "./someFilePath"

        DhallParser.parse(expression).get must equalTo(Expr.Embed(File("./someFilePath")))
      }

      "starting with ../" in {
        val expression = "../someFilePath"

        DhallParser.parse(expression).get must equalTo(Expr.Embed(File("../someFilePath")))
      }
    }
  }
}
