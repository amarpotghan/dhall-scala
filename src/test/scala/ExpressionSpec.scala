package dhall

import org.specs2.mutable._

import Expression._

class ExpressionSpec extends Specification {

  "shiftVariableIndices: should shift index if provided variable name matches with, " >> {
    "Lambda domain label" >> {
      val given = Lambda("x", IntegerType, Variable("x", 0))
      val shifted = given.shiftVariableIndices(1, Variable("x", 0))
      shifted mustEqual Lambda("x", IntegerType, Variable("x", 1))
    }

    "Let binding label" >> {
      // let x = 1 in x
      val given = Let("x", None, IntegerLit(1), Variable("x", 0))
      val shifted = given.shiftVariableIndices(1, Variable("x", 0))
      shifted mustEqual Let("x", None, IntegerLit(1), Variable("x", 1))
    }

    "Variable symbol name" >> {
      // x
      val given = Variable("x", 0)
      val shifted = given.shiftVariableIndices(1, Variable("x", 0))
      shifted mustEqual Variable("x", 1)
    }

    "Pi label" >> {
      val given = Pi("x", IntegerType, Variable("x", 0))
      val shifted = given.shiftVariableIndices(1, Variable("x", 0))
      shifted mustEqual Pi("x", IntegerType, Variable("x", 1))
    }

    "Var in Lam in Let" >> {
      // let x = True in \x: BoolType -> x && True
      val given = Let("x", Some(BoolType), BoolLit(true), Lambda("x", BoolType, BoolAnd(Variable("x", 0), BoolLit(true))))
      val shifted = given.shiftVariableIndices(1, Variable("x", 0))
      val expected = Let("x", Some(BoolType), BoolLit(true), Lambda("x", BoolType, BoolAnd(Variable("x", 1), BoolLit(true))))
      shifted mustEqual expected
    }
  }
  "substitute" >> {
    "should substitute variable with given expression in: " >> {
      "variable" >> {
        val given = Variable("x", 0)
        val expr = Let("y", None, BoolLit(true), Variable("y", 0))
        val substituted = given.substitute(Variable("x", 0), expr)
        substituted mustEqual expr
      }

      "Let expression" >> {
        val given = Let("x", None, BoolLit(true), BoolAnd(Variable("y", 0), Variable("x", 0)))
        val substituted = given.substitute(Variable("y", 0), BoolLit(true))
        substituted mustEqual Let("x", None, BoolLit(true), BoolAnd(BoolLit(true), Variable("x", 0)))
      }

      "Lamda expression" >> {
        val given = Lambda("x", BoolType, ListLit(None, Seq(Variable("y", 0), BoolLit(false))))
        val substituted = given.substitute(Variable("y", 0), BoolLit(true))
        substituted mustEqual Lambda("x", BoolType, ListLit(None, Seq(BoolLit(true), BoolLit(false))))
      }
    }

    "should not substitute if same variable name is bound to: " >> {
      "Domain of lamda expression" >> {
        val given = Lambda("x", BoolType, ListLit(None, Seq(Variable("x", 0), BoolLit(false))))
        val substituted = given.substitute(Variable("x", 0), BoolLit(true))
        substituted mustEqual given
      }

      "Let binding name" >> {
        val given = Let("x", Some(BoolType), BoolLit(true), ListLit(None, Seq(Variable("x", 0), BoolLit(false))))
        val substituted = given.substitute(Variable("x", 0), BoolLit(true))
        substituted mustEqual given
      }

      "Quant variable name" >> {
        val given = Pi("x", Const.Type, Variable("x", 0))
        val substituted = given.substitute(Variable("x", 0), Variable("y", 0))
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

      "Combine" >> {
        val innerRecord1 = RecordLit(Map("y1" -> NaturalPlus(Variable("x", 0), NaturalLit(1))))
        val innerRecord2 = RecordLit(Map("y2" -> NaturalTimes(Variable("x", 0), NaturalLit(2))))
        val expr1 = Application(Lambda("x", NaturalType, RecordLit(Map("x" -> Variable("x", 0), "y" -> innerRecord1))), NaturalLit(1))
        val expr2 = Application(Lambda("x", NaturalType, RecordLit(Map("z" -> Variable("x", 0), "y" -> innerRecord2))), NaturalLit(2))
        val combined = Combine(expr1, expr2).normalize
        combined mustEqual RecordLit(Map("y" -> RecordLit(Map("y1" -> NaturalLit(2), "y2" -> NaturalLit(4))), "x" -> NaturalLit(1), "z" -> NaturalLit(2)))
      }

      "Merge" >> {
        val functions = RecordLit(Map("Left" -> Lambda("y", NaturalType, Application(NaturalIsZero, Variable("y", 0))),
                                      "Right" -> Lambda("x", BoolType, Let("x", Some(BoolType), BoolLit(true), BoolAnd(Variable("x", 0), BoolLit(true))))))

        val union1 = UnionLit("Left", NaturalLit(0), Map("Right" -> BoolType))
        val union2 = UnionLit("Right", BoolLit(false), Map("Left" -> NaturalType))

        Merge(functions, union1, BoolType).normalize mustEqual BoolLit(true)
        Merge(functions, union2, BoolType).normalize mustEqual BoolLit(true)
      }

      "Field" >> {
        val record = RecordLit(Map("x" -> NaturalLit(1), "y" -> NaturalLit(2)))
        val existing = "y"
        val nonExisting = "z"

        Field(record, existing).normalize mustEqual NaturalLit(2)
        Field(record, nonExisting).normalize mustEqual Field(record, nonExisting)
      }

      "Application" >> {
        "Lamda" >> {
          Application(Lambda("x", NaturalType, NaturalTimes(Variable("x", 0), Variable("x", 0))), NaturalLit(2)).normalize mustEqual NaturalLit(4)
        }

        "ListBuild" >> {
          val ls =
            Application(
              Application(ListBuild, NaturalType),
              Lambda("l", Const.Type,
                  Lambda("cons",
                      Pi("_", NaturalType,
                            Pi("_", Variable("l", 0), Variable("l", 0))),
                      Lambda("nil", Variable("l", 0),
                          Application(Application(Variable("cons", 0), NaturalLit(1)), Variable("nil", 0))))))

          ls.normalize mustEqual ListLit(Some(NaturalType), Seq(NaturalLit(1)))
        }

        "ListFold" >> {
          val fold =
            Application(
              Application(
                Application(
                  Application(ListFold, NaturalType),
                  ListLit(Some(NaturalType), Seq(1, 2, 3, 4).map(NaturalLit(_)))),
                Lambda("x", NaturalType, Lambda("y", NaturalType, NaturalPlus(Variable("x", 0), Variable("y", 0))))),
              NaturalLit(0)).normalize

          fold mustEqual NaturalLit(10)
        }

        "ListReverse" >> {
          val reverseExpr = Application(Application(ListReverse, NaturalType), ListLit(None, Seq(1, 2, 3).map(NaturalLit(_))))
          reverseExpr.normalize mustEqual ListLit(Some(NaturalType), Seq(3, 2, 1).map(NaturalLit(_)))
        }

        "ListHead" >> {
          val headExpr = Application(Application(ListHead, NaturalType), ListLit(None, Seq(1, 2).map(NaturalLit(_))))
          headExpr.normalize mustEqual OptionalLit(NaturalType, Seq(NaturalLit(1)))
        }

        "ListLast" >> {
          val lastExpr = Application(Application(ListLast, NaturalType), ListLit(None, Seq(1, 2).map(NaturalLit(_))))
          lastExpr.normalize mustEqual OptionalLit(NaturalType, Seq(NaturalLit(2)))
        }

        "ListLength" >> {
          val lengthExpr = Application(Application(ListLength, NaturalType), ListLit(None, Seq(1, 2).map(NaturalLit(_))))
          lengthExpr.normalize mustEqual NaturalLit(2)
        }

        "ListIndexed" >> {
          val lengthExpr = Application(Application(ListIndexed, NaturalType), ListLit(None, Seq(1, 2).map(NaturalLit(_))))
          val record = Record(Map("index" -> NaturalType, "value" -> NaturalType))
          val expected = ListLit(
            Some(record),
            List(RecordLit(Map("index" -> NaturalLit(0), "value" -> NaturalLit(1))),
                 RecordLit(Map("index" -> NaturalLit(1), "value" -> NaturalLit(2)))))
          lengthExpr.normalize mustEqual expected
        }

        "NaturalFold" >> {
          val fold =
            Application(
              Application(
                Application(
                  Application(NaturalFold, NaturalLit(2)),
                  NaturalType),
                Lambda("x", NaturalType,
                    NaturalPlus(Variable("x", 0), NaturalLit(15)))), NaturalLit(1))
          fold.normalize mustEqual NaturalLit(31)
        }

        "NaturalBuild" >> {
          val ls = Application(NaturalBuild,
                       Lambda("natural", Const.Type,
                           Lambda("succ", Pi("_", Variable("natural", 0), Variable("natural", 0)),
                               Lambda("zero", Variable("natural", 0),
                                   Application(Variable("succ", 0), Application(Variable("succ", 0), Application(Variable("succ", 0), Application(Variable("succ", 0), Variable("zero", 0)))))))))

          ls.normalize mustEqual NaturalLit(4)
        }

        "NaturalIsZero" >> {
          Application(NaturalIsZero, NaturalLit(0)).normalize mustEqual BoolLit(true)
          Application(NaturalIsZero, NaturalLit(1)).normalize mustEqual BoolLit(false)
        }
        "NaturalEven" >> {
          Application(NaturalEven, NaturalLit(2)).normalize mustEqual BoolLit(true)
          Application(NaturalEven, NaturalLit(3)).normalize mustEqual BoolLit(false)
        }
        "NaturalOdd" >> {
          Application(NaturalOdd, NaturalLit(2)).normalize mustEqual BoolLit(false)
          Application(NaturalOdd, NaturalLit(3)).normalize mustEqual BoolLit(true)
        }

        "OptionalFold" >> {
          def foldApplication(value: Seq[Expression[String, Int]]) =
            Application(
              Application(
                Application(
                  Application(
                    Application(OptionalFold, NaturalType),
                    OptionalLit(NaturalType, value)),
                  NaturalType),
                Lambda("x", NaturalType, Variable("x", 0))),
              NaturalLit(0))

          "some case" >> {
            val optionalFold = foldApplication(Seq(NaturalLit(1)))
            optionalFold.normalize mustEqual NaturalLit(1)
          }

          "none case" >> {
            foldApplication(Nil).normalize mustEqual NaturalLit(0)
          }
        }
      }
    }
  }
}
