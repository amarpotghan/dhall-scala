package dhall

import dhall.Expression.{Embed, Lam, Let, ListLit, Quant, Union, UnionLit}
import org.specs2.matcher.Matchers
import org.specs2.mutable.Specification

class DhallParserSpec extends Specification with Matchers {

  "Dhall parser" should {

    "parse environment variable expressions" in {
      val expression = "env : PATH"

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
        val envAExpression = "env : pathA"
        val envBExpression = "env : pathB"

        val listLitExpression = s"[$envAExpression,$envBExpression ]"

        DhallParser.parse(listLitExpression).get must equalTo(ListLit(None, Seq(Embed(Env("pathA")), Embed(Env("pathB")))))
      }

      "parse List of environment expression and listLiteral" in {
        val listLitExpression = s"[env : pathA,env:pathB ]"
        val envCExpression = "env : pathC"

        val combinedListLitExpression = s"[$listLitExpression,$envCExpression ]"

        val listLit = ListLit(None, Seq(Embed(Env("pathA")), Embed(Env("pathB"))))
        val envC = Embed(Env("pathC"))

        DhallParser.parse(combinedListLitExpression).get must equalTo(ListLit(None, Seq(listLit, envC)))
      }
    }

    "parse lambda expressions" should {
      "with \\ prefix" in {
        val envAExpression = "env : pathA"
        val envBExpression = "env:pathB"
        val envCExpression = "env : pathC"

        val listLitExpression = s"[$envBExpression,$envCExpression ]"

        val label = "someLabel"

        val expression = s"\\($label : $envAExpression) -> $listLitExpression"

        DhallParser.parse(expression).get must
          equalTo(Lam(label, Embed(Env("pathA")), ListLit(None, Seq(Embed(Env("pathB")), Embed(Env("pathC"))))))
      }

      "with λ prefix" in {
        val envAExpression = "env:pathA"
        val envBExpression = "env : pathB"
        val envCExpression = "env:pathC"

        val listLitExpression = s"[$envBExpression,$envCExpression ]"

        val label = "someLabel"

        val expression = s"λ($label : $envAExpression) -> $listLitExpression"

        DhallParser.parse(expression).get must
          equalTo(Lam(label, Embed(Env("pathA")), ListLit(None, Seq(Embed(Env("pathB")), Embed(Env("pathC"))))))
      }
    }

    "parse quant expressions" should {
      "with forall prefix" in {
        val envAExpression = "env : pathA"
        val envBExpression = "env : pathB"
        val envCExpression = "env:pathC"

        val listLitExpression = s"[$envBExpression,$envCExpression ]"

        val label = "someLabel"

        val expression = s"forall($label:$envAExpression) -> $listLitExpression"

        DhallParser.parse(expression).get must
          equalTo(Quant(label, Embed(Env("pathA")), ListLit(None, Seq(Embed(Env("pathB")), Embed(Env("pathC"))))))
      }

      "with ∀ prefix" in {
        val envAExpression = "env : pathA"
        val envBExpression = "env : pathB"
        val envCExpression = "env : pathC"

        val listLitExpression = s"[$envBExpression,$envCExpression ]"

        val label = "someLabel"

        val expression = s"∀($label:$envAExpression) -> $listLitExpression"

        DhallParser.parse(expression).get must
          equalTo(Quant(label, Embed(Env("pathA")), ListLit(None, Seq(Embed(Env("pathB")), Embed(Env("pathC"))))))
      }
    }

    "parse let expressions" should {
      "with type" in {
        val envAExpression = "env:pathA"
        val envBExpression = "env:pathB"
        val envCExpression = "env:pathC"

        val label = "someLabel"

        val expression = s"let $label : $envAExpression = $envBExpression in $envCExpression"

        DhallParser.parse(expression).get must
          equalTo(Let(label, Some(Embed(Env("pathA"))), Embed(Env("pathB")), Embed(Env("pathC"))))
      }

      "without type" in {
        val envAExpression = "env : pathA"
        val envBExpression = "env:pathB"

        val label = "someLabel"

        val expression = s"let $label = $envAExpression in $envBExpression"

        DhallParser.parse(expression).get must
          equalTo(Let(label, None, Embed(Env("pathA")), Embed(Env("pathB"))))
      }
    }

    "Union expressions" should {
      "parse Union of environment expressions" in {
        val envAExpression = "a : env : pathA"
        val envBExpression = "b : env : pathB"

        val unionExpression = s"< $envAExpression|$envBExpression>"

        DhallParser.parse(unionExpression).get must equalTo(Union(Map(("a", Embed(Env("pathA"))), ("b", Embed(Env("pathB"))))))
      }

      "parse Union of environment expression and listLiteral" in {
        val listLitExpression = s"list : [env : pathA,env:pathB ]"
        val envCExpression = "c : env:pathC"

        val combinedUnionExpression = s"< $listLitExpression|$envCExpression>"

        val listLit = ListLit(None, Seq(Embed(Env("pathA")), Embed(Env("pathB"))))
        val envC = Embed(Env("pathC"))

        DhallParser.parse(combinedUnionExpression).get must equalTo(Union(Map(("list", listLit), ("c", envC))))
      }
    }

    "UnionLiteral expressions" should {
      "parse UnionLiteral of environment expressions" should {
        "without alternative types" in {
          val envAExpression = "a=env:pathA"
          val unionExpression = s"< $envAExpression>"

          DhallParser.parse(unionExpression).get must equalTo(UnionLit("a", Embed(Env("pathA")), Map.empty))
        }

        "with alternative types" in {
          val envAExpression = "a=env : pathA"
          val envBExpression = "b : env:pathB"
          val envCExpression = "c:env:pathC"
          val unionExpression = s"< $envAExpression|$envBExpression|$envCExpression>"

          DhallParser.parse(unionExpression).get must
            equalTo(UnionLit("a", Embed(Env("pathA")), Map(("b", Embed(Env("pathB"))), ("c", Embed(Env("pathC"))))))

        }
      }

      "parse UnionLiteral of listLiteral expressions" should {
        "without alternative types" in {
          val listLitExpression = s"list=[env:pathA,env:pathB ]"

          val unionExpression = s"< $listLitExpression>"

          val listLit = ListLit(None, Seq(Embed(Env("pathA")), Embed(Env("pathB"))))

          DhallParser.parse(unionExpression).get must equalTo(UnionLit("list", listLit, Map.empty))
        }

        "with alternative types" in {
          val listLitExpression = s"list=[env : pathA ]"
          val envBExpression = "b : env : pathB"
          val envCExpression = "c : env:pathC"

          val unionExpression = s"< $listLitExpression|$envBExpression|$envCExpression>"

          val listLit = ListLit(None, Seq(Embed(Env("pathA"))))

          DhallParser.parse(unionExpression).get must
            equalTo(UnionLit("list", listLit, Map(("b", Embed(Env("pathB"))), ("c", Embed(Env("pathC"))))))
        }
      }
    }
  }
}
