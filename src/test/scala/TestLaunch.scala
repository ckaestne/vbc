package edu.cmu.cs.vbc

import org.scalatest.{Ignore, FunSuite}


/**
  * simple starter, checks for successful execution without crashes, no assertion checking
  */
@Ignore
class TestLaunch extends FunSuite with DiffTestInfrastructure {

    test("ifelse") {
        Launcher.launch("edu.cmu.cs.vbc.prog.IfElseExample")
    }
    test("bankaccount") {
        Launcher.launch("edu.cmu.cs.vbc.prog.bankaccount.Main")
    }

}
