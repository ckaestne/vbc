package edu.cmu.cs.vbc

import edu.cmu.cs.vbc.instructions._
import edu.cmu.cs.vbc.test.{InstrDBGIPrint, InstrLoadConfig}
import org.objectweb.asm.Opcodes._
import org.scalatest.FunSuite


class VBCTest extends FunSuite with DiffTestInfrastructure {

    private def simpleMethod(instrs: Instruction*) =
        testMethod(new MethodNode(ACC_PUBLIC, "test", "()V", "()V", Array.empty,
            CFG(List(Block(instrs.toList)))))

    test("simple method") {
        simpleMethod(InstrICONST(), InstrDBGIPrint(), InstrRETURN())
    }

    test("load condition") {
        simpleMethod(InstrLoadConfig("A"), InstrDBGIPrint(), InstrRETURN())
    }

    test("conditional IADD") {
        simpleMethod(InstrICONST(), InstrLoadConfig("A"), InstrIADD(), InstrDBGIPrint(), InstrRETURN())
    }


    test("2 conditional IADD") {
        simpleMethod(InstrLoadConfig("B"), InstrLoadConfig("A"), InstrIADD(), InstrDBGIPrint(), InstrRETURN())
    }


}
