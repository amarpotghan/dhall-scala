package dhall

import dhall.Expr.{IntegerType, _}
import dhall.PrettyPrinter._
import org.specs2.matcher.Matchers
import org.specs2.mutable.Specification

class PrettyPrinterSpec extends Specification with Matchers {

  "PrettyPrinter" should {

    "print a Record" in {
      prettyPrint(Record(
        Map("x" -> IntegerType, "y" -> StringType, "z" -> OptionalType)
      )) must equalTo("{ x : Integer, y : String, z : Optional }")

      prettyPrint(RecordLit(
        Map("x" -> IntegerLit(0), "y" -> StringLit("text"), "z" -> OptionalLit(DoubleType, Seq(DoubleLit(1.0), DoubleLit(2.0))))
      )) must equalTo("{x = 0, y = text, z = [1.0, 2.0] : Optional Double}")
    }

    "print a simple lambda" in {
      prettyPrint(Lam("x", IntegerType, Var("b", 0))) must equalTo("λ(x : Integer) → b")
      prettyPrint(Lam("x", BoolType, Var("b", 0))) must equalTo("λ(x : Bool) → b")
      prettyPrint(Lam("x", NaturalType, Var("b", 0))) must equalTo("λ(x : Natural) → b")
      prettyPrint(Lam("x", DoubleType, Var("b", 0))) must equalTo("λ(x : Double) → b")
      prettyPrint(Lam("x", StringType, Var("b", 0))) must equalTo("λ(x : String) → b")
      prettyPrint(Lam("x", ListType, Var("b", 0))) must equalTo("λ(x : List) → b")
      prettyPrint(Lam("x", OptionalType, Var("b", 0))) must equalTo("λ(x : Optional) → b")
    }

    "print a nested lambda" in {
      val lam = Lam("x", Const.Type, Lam("const", IntegerType, Var("list", 0)))
      prettyPrint(lam) must equalTo("λ(x : Type) → λ(const : Integer) → list")
    }

    "print an if condition" in {
      val eq = BoolEQ(Var("x", 0), IntegerLit(10))
      val and = BoolAnd(Var("x", 0), IntegerLit(10))
      val or = BoolOr(Var("x", 0), IntegerLit(10))
      val ne = BoolNE(Var("x", 0), IntegerLit(10))

      val boolIf1 = BoolIf(eq, Var("y", 0), Var("z", 0))
      val boolIf2 = BoolIf(and, Var("y", 0), Var("z", 0))
      val boolIf3 = BoolIf(or, Var("y", 0), Var("z", 0))
      val boolIf4 = BoolIf(ne, Var("y", 0), Var("z", 0))

      prettyPrint(boolIf1) must equalTo("if x == 10 then y else z")
      prettyPrint(boolIf2) must equalTo("if x && 10 then y else z")
      prettyPrint(boolIf3) must equalTo("if x || 10 then y else z")
      prettyPrint(boolIf4) must equalTo("if x != 10 then y else z")
    }

    "print a Record Combine" in {
      prettyPrint(Combine(
        RecordLit(Map("x" -> IntegerLit(1), "y" -> StringLit("text"))),
        RecordLit(Map("z" -> BoolLit(true))))) must equalTo("{x = 1, y = text} ∧ {z = True}")
    }

    "print a Let expression" in {
      prettyPrint(Let(
        "twice",
        None,
        Lam("x", StringType, StringAppend(StringLit("x"), StringLit("x"))),
        StringLit("twice ha"))
      ) must equalTo("let twice = λ(x : String) → x ++ x in twice ha")
    }

    "print a Union" in {

      prettyPrint(Union(
        Map("Left" -> StringType, "Right" -> NaturalType))
      ) must equalTo("< Left : String | Right : Natural >")

      prettyPrint(UnionLit(
        "h",
        NaturalEven,
        Map("Left" -> StringType, "Right" -> NaturalType))
      ) must equalTo("< h = Natural/even | Left : String | Right : Natural >")
    }

    "print a for all" in {
      prettyPrint(Quant("_", Var("a", 0), Var("b", 0))) must equalTo("a → b")
      prettyPrint(Quant("x", Var("a", 0), Var("b", 0))) must equalTo("∀(x : a) → b")
    }

    "print a let expression" in {
      prettyPrint(Let("x", None, Var("2a", 0), Var("4x", 0))) must equalTo("let x = 2a in 4x")
      prettyPrint(Let("x", Some(IntegerType), Var("2a", 0), Var("4x", 0))) must equalTo("let x : Integer = 2a in 4x")
    }

    "print a list lit" in {
      prettyPrint(ListLit(None, Seq(NaturalLit(1), NaturalLit(2)))) must equalTo("[+1, +2]")
      prettyPrint(ListLit(Some(IntegerType), Seq(IntegerLit(1), IntegerLit(2)))) must equalTo("[1, 2] : List Integer")
    }

    "print an App" in {
      prettyPrint(App(
        Lam("x", Const.Type, Lam("const", IntegerType, Var("list", 0))),
        IntegerType)
      ) must equalTo("(λ(x : Type) → λ(const : Integer) → list) Integer")
    }


  }
}
