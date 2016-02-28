package edu.cmu.cs.vbc

import org.scalatest.{FunSuite, Ignore}


/**
  * simple starter, checks for successful execution without crashes, no assertion checking
  */
@Ignore
class TestLaunch extends FunSuite with DiffTestInfrastructure {

    test("ifelse") {
        VBCLauncher.launch("edu.cmu.cs.vbc.prog.IfElseExample")
    }
    test("bankaccount") {
        VBCLauncher.launch("edu.cmu.cs.vbc.prog.bankaccount.Main")
    }

}
