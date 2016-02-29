package edu.cmu.cs.vbc

import de.fosd.typechef.featureexpr.FeatureExprFactory
import edu.cmu.cs.vbc.test.{InstrDBGIPrint, InstrLoadConfig}
import edu.cmu.cs.vbc.vbytecode._
import edu.cmu.cs.vbc.vbytecode.instructions._
import org.objectweb.asm.Opcodes._
import org.scalatest.FunSuite


class VBCInstrTest extends FunSuite with DiffTestInfrastructure {
    FeatureExprFactory.setDefault(FeatureExprFactory.bdd)

    private def simpleMethod(instrs: Instruction*) =
        testMethod(new VBCMethodNode(ACC_PUBLIC, "test", "()V", "()V", Nil,
            CFG(List(Block(instrs: _*)))))

    test("simple method") {
        simpleMethod(InstrICONST(0), InstrDBGIPrint(), InstrRETURN())
    }

    test("load condition") {
        simpleMethod(InstrLoadConfig("A"), InstrDBGIPrint(), InstrRETURN())
    }

    test("conditional IADD") {
        simpleMethod(InstrICONST(4), InstrLoadConfig("A"), InstrIADD(), InstrDBGIPrint(), InstrRETURN())
    }


    test("2 conditional IADD") {
        simpleMethod(InstrLoadConfig("B"), InstrLoadConfig("A"), InstrIADD(), InstrDBGIPrint(), InstrRETURN())
    }

    test("LOAD, STORE") {
        val l = new LocalVar()
        simpleMethod(InstrLoadConfig("A"), InstrISTORE(l), InstrILOAD(l), InstrDBGIPrint(), InstrRETURN())
    }

    test("LOAD, STORE, IINC") {
        val l = new LocalVar()
        simpleMethod(InstrLoadConfig("A"), InstrISTORE(l), InstrIINC(l, 1), InstrILOAD(l), InstrDBGIPrint(), InstrRETURN())
    }

}
