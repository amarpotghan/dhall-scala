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
}
