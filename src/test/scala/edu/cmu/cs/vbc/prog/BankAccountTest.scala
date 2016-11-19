package edu.cmu.cs.vbc.prog

import de.fosd.typechef.featureexpr.FeatureExprFactory
import edu.cmu.cs.vbc.DiffLaunchTestInfrastructure
import org.scalatest.FunSuite

/**
  * @author chupanw
  */
class BankAccountTest extends FunSuite with DiffLaunchTestInfrastructure {

  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)

  test("bankaccount") {
    testMain(classOf[edu.cmu.cs.vbc.prog.bankaccount.Main])
  }

  test("bankaccout2") {
    testMain(classOf[edu.cmu.cs.vbc.prog.bankaccount2.Main])
  }
}
