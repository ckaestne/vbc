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

  test("fieldtest") {
    testMain("edu.cmu.cs.vbc.prog.FieldTest")
  }

  test("invoke example") {
    testMain("edu.cmu.cs.vbc.prog.InvokeExample")
  }

  test("unbalanced stack example") {
    testMain("edu.cmu.cs.vbc.prog.UnbalancedStackExample")
  }

  test("bankaccount") {
    checkCrash("edu.cmu.cs.vbc.prog.bankaccount.Main")
  }

  test("static fields") {
    testMain("edu.cmu.cs.vbc.prog.StaticFieldsExample")
  }

  test("static fields with clinit") {
    testMain("edu.cmu.cs.vbc.prog.StaticFieldsWithClinit")
  }

  test("bankaccout2") {
    checkCrash("edu.cmu.cs.vbc.prog.bankaccount2.Main")
  }

}
