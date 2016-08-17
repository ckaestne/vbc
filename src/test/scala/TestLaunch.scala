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
    testMain(classOf[edu.cmu.cs.vbc.prog.bankaccount.Main])
  }

  test("static fields") {
    //    testMain(classOf[edu.cmu.cs.vbc.prog.StaticFieldsExample])  //todo
    testMain(classOf[edu.cmu.cs.vbc.prog.StaticFieldsExample], compareTraceAgainstBruteForce = false)
  }

  test("static fields with clinit") {
    //    testMain(classOf[edu.cmu.cs.vbc.prog.StaticFieldsWithClinit]) //todo
    testMain(classOf[edu.cmu.cs.vbc.prog.StaticFieldsWithClinit], compareTraceAgainstBruteForce = false)
  }

  test("bankaccout2") {
    //    testMain(classOf[edu.cmu.cs.vbc.prog.bankaccount2.Main])
    testMain(classOf[edu.cmu.cs.vbc.prog.bankaccount2.Main], compareTraceAgainstBruteForce = false)
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

  test("int array") {
    testMain(classOf[edu.cmu.cs.vbc.prog.IntArrayExample])
  }

  test("char array") {
    testMain(classOf[edu.cmu.cs.vbc.prog.CharArrayExample])
  }

  test("gpl") {
    testMain(classOf[edu.cmu.cs.vbc.prog.gpl.Main], compareTraceAgainstBruteForce = false)
    //    testMain(classOf[edu.cmu.cs.vbc.prog.gpl.Main]) //todo
  }

  test("LinkedList") {
    //    testMain(classOf[edu.cmu.cs.vbc.prog.LinkedListTest]) //todo
    testMain(classOf[edu.cmu.cs.vbc.prog.LinkedListTest], compareTraceAgainstBruteForce = false)
  }
}
