package edu.cmu.cs.vbc

import de.fosd.typechef.featureexpr.FeatureExprFactory
import org.scalatest.FunSuite


/**
  * simple starter, checks for successful execution without crashes, no assertion checking
  */
class TestLaunch extends FunSuite with DiffLaunchTestInfrastructure {

    FeatureExprFactory.setDefault(FeatureExprFactory.bdd)

    test("ifelse") {
        testMain("edu.cmu.cs.vbc.prog.IfElseExample")
    }

    test("test1") {
        testMain("edu.cmu.cs.vbc.prog.Test1")
    }

    ignore("bankaccount") {
        testMain("edu.cmu.cs.vbc.prog.bankaccount.Main")
    }

}
