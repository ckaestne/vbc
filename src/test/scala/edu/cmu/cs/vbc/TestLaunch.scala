package edu.cmu.cs.vbc

import de.fosd.typechef.featureexpr.FeatureExprFactory
import edu.cmu.cs.vbc.prog._
import edu.cmu.cs.vbc.prog.elevator.PL_Interface_impl
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
    testMain(classOf[edu.cmu.cs.vbc.prog.StaticFieldsExample])
  }

  test("static fields with clinit") {
    testMain(classOf[edu.cmu.cs.vbc.prog.StaticFieldsWithClinit])
  }

  test("bankaccout2") {
    testMain(classOf[edu.cmu.cs.vbc.prog.bankaccount2.Main])
  }

  test("conditional field assignment") {
    testMain(classOf[edu.cmu.cs.vbc.prog.FieldTest])
  }

  test("conditional method invocations") {
    testMain(classOf[edu.cmu.cs.vbc.prog.MethodTest])
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

  ignore("gpl") {
    testMain(classOf[edu.cmu.cs.vbc.prog.gpl.Main], compareTraceAgainstBruteForce = false)
    //    testMain(classOf[edu.cmu.cs.vbc.prog.gpl.Main]) //todo
  }

  ignore("LinkedList") {
    //    testMain(classOf[edu.cmu.cs.vbc.prog.LinkedListTest]) //todo
    testMain(classOf[LinkedListExample], compareTraceAgainstBruteForce = false)
  }

  test("Different ways of superclass initialization") {
    testMain(classOf[InitExample])
  }

  test("Exceptions") {
    testMain(classOf[ExceptionExample], compareTraceAgainstBruteForce = false)
  }

  test("StringBuilderExample") {
    testMain(classOf[StringBuilderExample])
  }

  test("ArrayListExample") {
    testMain(classOf[ArrayListExample])
  }

  test("Elevator") {
    testMain(classOf[PL_Interface_impl])
  }

  test("Email") {
    testMain(classOf[email.PL_Interface_impl])
  }
}
