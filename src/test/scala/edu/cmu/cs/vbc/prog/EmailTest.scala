package edu.cmu.cs.vbc.prog

import de.fosd.typechef.featureexpr.FeatureExprFactory
import edu.cmu.cs.vbc.DiffLaunchTestInfrastructure
import org.scalatest.FunSuite

/**
  * @author chupanw
  */
class EmailTest extends FunSuite with DiffLaunchTestInfrastructure {
  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)
  test("Email") {
    testMain(classOf[email.PL_Interface_impl])
  }
}
