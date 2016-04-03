package edu.cmu.cs.vbc

import de.fosd.typechef.featureexpr.FeatureExprFactory
import edu.cmu.cs.vbc.test.{InstrDBGIPrint, InstrLoadConfig}
import edu.cmu.cs.vbc.vbytecode._
import edu.cmu.cs.vbc.vbytecode.instructions._
import org.objectweb.asm.Opcodes._
import org.scalatest.FunSuite


class VBCInstrTest extends FunSuite with DiffMethodTestInfrastructure {
  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)

  private def simpleMethod(instrs: Instruction*) =
    testMethod(new VBCMethodNode(ACC_PUBLIC, "test", "()V", Some("()V"), Nil,
      CFG(List(Block(instrs: _*)))))

  test("simple method") {
    simpleMethod(InstrICONST(0), InstrDBGIPrint(), InstrRETURNVoid())
  }

  test("load condition") {
    simpleMethod(InstrLoadConfig("A"), InstrDBGIPrint(), InstrRETURNVoid())
  }

  test("conditional IADD") {
    simpleMethod(InstrICONST(4), InstrLoadConfig("A"), InstrIADD(), InstrDBGIPrint(), InstrRETURNVoid())
  }


  test("2 conditional IADD") {
    simpleMethod(InstrLoadConfig("B"), InstrLoadConfig("A"), InstrIADD(), InstrDBGIPrint(), InstrRETURNVoid())
  }

  test("LOAD, STORE") {
    val l = new LocalVar("v", "I")
    simpleMethod(InstrLoadConfig("A"), InstrISTORE(l), InstrILOAD(l), InstrDBGIPrint(), InstrRETURNVoid())
  }

  test("LOAD, STORE, IINC") {
    val l = new LocalVar("v", "I")
    simpleMethod(InstrLoadConfig("A"), InstrISTORE(l), InstrIINC(l, 1), InstrILOAD(l), InstrDBGIPrint(), InstrRETURNVoid())
  }

}
