package edu.cmu.cs.vbc.prog

import de.fosd.typechef.featureexpr.FeatureExprFactory
import edu.cmu.cs.vbc.DiffLaunchTestInfrastructure
import org.scalatest.FunSuite

/**
  * @author chupanw
  */
class GPLTest extends FunSuite with DiffLaunchTestInfrastructure{
  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)
  test("GPL") {
    testMain(classOf[gpl.Main])
  }
}
