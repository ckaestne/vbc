package edu.cmu.cs.vbc

import org.scalatest.FunSuite


/**
  * simple starter, checks for successful execution without crashes, no assertion checking
  */
class TestLaunch extends FunSuite with DiffTestInfrastructure {

    test("ifelse") {
        VBCLauncher.launch("edu.cmu.cs.vbc.prog.IfElseExample")
    }
    ignore("bankaccount") {
        VBCLauncher.launch("edu.cmu.cs.vbc.prog.bankaccount.Main")
    }

}
