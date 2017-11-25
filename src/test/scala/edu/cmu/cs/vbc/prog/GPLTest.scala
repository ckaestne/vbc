package edu.cmu.cs.vbc.prog

import com.sun.org.apache.bcel.internal.generic.ATHROW
import de.fosd.typechef.featureexpr.FeatureExprFactory
import edu.cmu.cs.vbc.analysis.VBCFrame
import edu.cmu.cs.vbc.vbytecode.instructions._
import edu.cmu.cs.vbc._
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.{MethodVisitor, Opcodes}
import org.objectweb.asm.Opcodes.{ILOAD, INVOKESTATIC, ISTORE}
import org.scalatest.FunSuite

/**
  * @author chupanw
  */
class GPLTest extends FunSuite with DiffLaunchTestInfrastructure {
  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)
  test("GPL") {
    testMain(classOf[gpl.Main], configFile = Some("gpl.conf"))
  }

  test("Mini") {
    testMain(classOf[gpl.Mini], configFile = Some("gpl.conf"), executeLifted = true, executeLiftedSilent=false,  compareTraceAgainstBruteForce = true, runBenchmark=false)
  }



}