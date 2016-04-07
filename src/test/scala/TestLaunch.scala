package edu.cmu.cs.vbc

import de.fosd.typechef.featureexpr.FeatureExprFactory
import org.scalatest.FunSuite


/**
  * simple starter, checks for successful execution without crashes, no assertion checking
  */
class TestLaunch extends FunSuite with DiffLaunchTestInfrastructure {

  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)

  test("ifelse") {
    testMain(classOf[edu.cmu.cs.vbc.prog.IfElseExample])
  }

  test("test1") {
    testMain(classOf[edu.cmu.cs.vbc.prog.Test1])
  }


  test("invoke example") {
    testMain(classOf[edu.cmu.cs.vbc.prog.InvokeExample])
  }

  test("unbalanced stack example") {
    testMain(classOf[edu.cmu.cs.vbc.prog.UnbalancedStackExample])
  }

  test("bankaccount") {
    checkCrash(classOf[edu.cmu.cs.vbc.prog.bankaccount.Main])
  }

  test("static fields") {
    testMain(classOf[edu.cmu.cs.vbc.prog.StaticFieldsExample])
  }

  test("static fields with clinit") {
    testMain(classOf[edu.cmu.cs.vbc.prog.StaticFieldsWithClinit])
  }

  test("bankaccout2") {
    checkCrash(classOf[edu.cmu.cs.vbc.prog.bankaccount2.Main])
  }

  test("conditional field assignment") {
    testMain(classOf[edu.cmu.cs.vbc.prog.FieldTest], true, false)
  }

  test("conditional method invocations") {
    testMain(classOf[edu.cmu.cs.vbc.prog.MethodTest], true, false)
  }

  test("reference array") {
    testMain(classOf[edu.cmu.cs.vbc.prog.RefArrayExample])
  }
}
