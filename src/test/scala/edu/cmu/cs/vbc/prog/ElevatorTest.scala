package edu.cmu.cs.vbc.prog

import de.fosd.typechef.featureexpr.FeatureExprFactory
import edu.cmu.cs.vbc.DiffLaunchTestInfrastructure
import org.scalatest.FunSuite

/**
  * @author chupanw
  */
class ElevatorTest extends FunSuite with DiffLaunchTestInfrastructure {
  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)
  test("Elevator") {
    testMain(classOf[elevator.PL_Interface_impl])
  }
}
