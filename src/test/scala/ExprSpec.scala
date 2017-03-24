package dhall

import org.specs2.mutable._


import Expr._

class ExprSpec extends Specification {

  "shiftVariableIndices: should shift index if provided variable name matches with, " >> {
    "Lam domain label" >> {
      // \x: IntegerType -> x
      val given = Lam("x", IntegerType, Var("x", 0))
      val shifted = given.shiftVariableIndices(1, Var("x", 0))
      shifted mustEqual Lam("x", IntegerType, Var("x", 1))
    }

    "Let binding label" >> {
      // let x = 1 in x
      val given = Let("x", None, IntegerLit(1), Var("x", 0))
      val shifted = given.shiftVariableIndices(1, Var("x", 0))
      shifted mustEqual Let("x", None, IntegerLit(1), Var("x", 1))
    }

    "Var name" >> {
      // x
      val given = Var("x", 0)
      val shifted = given.shiftVariableIndices(1, Var("x", 0))
      shifted mustEqual Var("x", 1)
    }

    "Quant label" >> {
      // forall(a: Int) -> a
      val given = Quant("x", IntegerType, Var("x", 0))
      val shifted = given.shiftVariableIndices(1, Var("x", 0))
      shifted mustEqual Quant("x", IntegerType, Var("x", 1))
    }

    "Var in Lam in Let" >> {
      // let x = True in \x: BoolType -> x && True
      val given = Let("x", Some(BoolType), BoolLit(true), Lam("x", BoolType, BoolAnd(Var("x", 0), BoolLit(true))))
      val shifted = given.shiftVariableIndices(1, Var("x", 0))
      val expected = Let("x", Some(BoolType), BoolLit(true), Lam("x", BoolType, BoolAnd(Var("x", 1), BoolLit(true))))
      shifted mustEqual expected
    }
  }
  "substitute" >> {
    "should substitute variable with given expression in: " >> {
      "variable" >> {
        val given = Var("x", 0)
        val expr = Let("y", None, BoolLit(true), Var("y", 0))
        val substituted = given.substitute(Var("x", 0), expr)
        substituted mustEqual expr
      }

      "Let expression" >> {
        val given = Let("x", None, BoolLit(true), BoolAnd(Var("y", 0), Var("x", 0)))
        val substituted = given.substitute(Var("y", 0), BoolLit(true))
        substituted mustEqual Let("x", None, BoolLit(true), BoolAnd(BoolLit(true), Var("x", 0)))
      }

      "Lamda expression" >> {
        val given = Lam("x", BoolType, ListLit(None, Seq(Var("y", 0), BoolLit(false))))
        val substituted = given.substitute(Var("y", 0), BoolLit(true))
        substituted mustEqual Lam("x", BoolType, ListLit(None, Seq(BoolLit(true), BoolLit(false))))
      }
    }

    "should not substitute if same variable name is bound to: " >> {
      "Domain of lamda expression" >> {
        val given = Lam("x", BoolType, ListLit(None, Seq(Var("x", 0), BoolLit(false))))
        val substituted = given.substitute(Var("x", 0), BoolLit(true))
        substituted mustEqual given
      }

      "Let binding name" >> {
        val given = Let("x", Some(BoolType), BoolLit(true), ListLit(None, Seq(Var("x", 0), BoolLit(false))))
        val substituted = given.substitute(Var("x", 0), BoolLit(true))
        substituted mustEqual given
      }

      "Quant variable name" >> {
        val given = Quant("x", Const.Type, Var("x", 0))
        val substituted = given.substitute(Var("x", 0), Var("y", 0))
        substituted mustEqual given
      }
    }
  }

  "normalize" >> {
    "should normalize expression with " >> {
      "Literal bool ops " >> {
        val lit1 = BoolLit(true)
        val lit2 = BoolLit(false)
        "BoolAnd" >> { BoolAnd(lit1, lit2).normalize mustEqual lit2 }
        "BoolOr" >> { BoolOr(lit1, lit2).normalize mustEqual lit1 }
        "BoolNE" >> { BoolNE(lit1, lit2).normalize mustEqual lit1 }
        "BoolEQ" >> { BoolEQ(lit1, lit2).normalize mustEqual lit2 }
        "BoolIf" >> { BoolIf(lit1, IntegerLit(1), IntegerLit(2)).normalize mustEqual IntegerLit(1) }
      }
      "Literal natural ops " >> {
        val lit1 = NaturalLit(1)
        val lit2 = NaturalLit(2)
        "NaturalPlus" >> { NaturalPlus(lit1, lit2).normalize mustEqual NaturalLit(3) }
        "NaturalTimes" >> { NaturalTimes(lit1, lit2).normalize mustEqual NaturalLit(2) }
      }
    }
  }
}
