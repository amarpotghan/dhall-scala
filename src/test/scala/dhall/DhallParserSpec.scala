package dhall

import dhall.Expr.{Embed, Lam, ListLit}
import org.specs2.matcher.Matchers
import org.specs2.mutable.Specification

class DhallParserSpec extends Specification with Matchers {

  "Dhall parser" should {

    "parse environment variable expressions" in {
      val expression = "env:PATH"

      DhallParser.parse(expression).get must equalTo(Embed(Env("PATH")))
    }

    "parse url expressions" should {
      "http url" in {
        val expression = "http://www.website.com:8080/path/to/resource?a=b&x=y#L726"

        DhallParser.parse(expression).get must equalTo(Embed(Url("http://www.website.com:8080/path/to/resource?a=b&x=y#L726")))
      }

      "https url" in {
        val expression = "https://www.website.com:8080/path/to/resource?a=b&x=y#L726"

        DhallParser.parse(expression).get must equalTo(Embed(Url("https://www.website.com:8080/path/to/resource?a=b&x=y#L726")))
      }
    }

    "parse file expressions" should {
      "starting with /" in {
        val expression = "/someFilePath"

        DhallParser.parse(expression).get must equalTo(Embed(File("/someFilePath")))
      }

      "starting with ./" in {
        val expression = "./someFilePath"

        DhallParser.parse(expression).get must equalTo(Embed(File("./someFilePath")))
      }

      "starting with ../" in {
        val expression = "../someFilePath"

        DhallParser.parse(expression).get must equalTo(Embed(File("../someFilePath")))
      }
    }

    "ListLiteral expressions" should {
      "parse List of environment expressions" in {
        val envAExpression = "env:pathA"
        val envBExpression = "env:pathB"

        val listLitExpression = s"[$envAExpression,$envBExpression]"

        DhallParser.parse(listLitExpression).get must equalTo(ListLit(None, Seq(Embed(Env("pathA")), Embed(Env("pathB")))))
      }

      "parse List of environment expression and listLiteral" in {
        val listLitExpression = s"[env:pathA,env:pathB]"
        val envCExpression = "env:pathC"

        val combinedListLitExpression = s"[$listLitExpression,$envCExpression]"

        val listLit = ListLit(None, Seq(Embed(Env("pathA")), Embed(Env("pathB"))))
        val envC = Embed(Env("pathC"))

        DhallParser.parse(combinedListLitExpression).get must equalTo(ListLit(None, Seq(listLit, envC)))
      }
    }

    "parse lambda expressions" should {
      "with \\ prefix" in {
        val envAExpression = "env:pathA"
        val envBExpression = "env:pathB"
        val envCExpression = "env:pathC"

        val listLitExpression = s"[$envBExpression,$envCExpression]"

        val label = "someLabel"

        val expression = s"\\($label:$envAExpression)->$listLitExpression"

        DhallParser.parse(expression).get must
          equalTo(Lam(label, Embed(Env("pathA")), ListLit(None, Seq(Embed(Env("pathB")), Embed(Env("pathC"))))))
      }

      "with λ prefix" in {
        val envAExpression = "env:pathA"
        val envBExpression = "env:pathB"
        val envCExpression = "env:pathC"

        val listLitExpression = s"[$envBExpression,$envCExpression]"

        val label = "someLabel"

        val expression = s"λ($label:$envAExpression)->$listLitExpression"

        DhallParser.parse(expression).get must
          equalTo(Lam(label, Embed(Env("pathA")), ListLit(None, Seq(Embed(Env("pathB")), Embed(Env("pathC"))))))
      }
    }
  }
}
