package edu.cmu.cs.vbc

import org.scalatest.FunSuite


/**
  * simple starter, checks for successful execution without crashes, no assertion checking
  */
class TestLaunch extends FunSuite with DiffLaunchTestInfrastructure {

    test("ifelse") {
        testMain("edu.cmu.cs.vbc.prog.IfElseExample")
    }

    ignore("bankaccount") {
        testMain("edu.cmu.cs.vbc.prog.bankaccount.Main")
    }

}
