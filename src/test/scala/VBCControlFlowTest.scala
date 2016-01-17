package edu.cmu.cs.vbc

import edu.cmu.cs.vbc.instructions._
import edu.cmu.cs.vbc.test.{InstrDBGIPrint, InstrLoadConfig}
import org.objectweb.asm.Opcodes._
import org.scalatest.FunSuite


class VBCControlFlowTest extends FunSuite with DiffTestInfrastructure {

    private def method(blocks: Block*) =
        testMethod(new MethodNode(ACC_PUBLIC, "test", "()V", "()V", Array.empty,
            CFG(blocks.toList)))

    test("basic if-then") {
        method(
            Block(InstrICONST(0), InstrIFEQ(2)),
            Block(InstrICONST(3), InstrDBGIPrint()),
            Block(InstrICONST(4), InstrDBGIPrint(), InstrRETURN())
        )
    }
    test("basic if-else") {
        method(
            Block(InstrICONST(1), InstrIFEQ(2)),
            Block(InstrICONST(3), InstrDBGIPrint()),
            Block(InstrICONST(4), InstrDBGIPrint(), InstrRETURN())
        )
    }
    test("conditional if") {
        method(
            Block(InstrLoadConfig("A"), InstrIFEQ(2)),
            Block(InstrICONST(3), InstrDBGIPrint()),
            Block(InstrICONST(4), InstrDBGIPrint(), InstrRETURN())
        )
    }

    test("nested conditional if") {
        method(
            Block(InstrLoadConfig("A"), InstrIFEQ(3)),
            Block(InstrLoadConfig("B"), InstrIFEQ(3)),
            Block(InstrICONST(3), InstrDBGIPrint()),
            Block(InstrICONST(4), InstrDBGIPrint(), InstrRETURN())
        )
    }

    test("nested conditional if2") {
        method(
            Block(InstrLoadConfig("A"), InstrIFEQ(2)),
            Block(InstrLoadConfig("B"), InstrIFEQ(3)),
            Block(InstrICONST(3), InstrDBGIPrint(), InstrGOTO(4)),
            Block(InstrICONST(3), InstrDBGIPrint()),
            Block(InstrICONST(4), InstrDBGIPrint(), InstrRETURN())
        )
    }

    test("conditional store") {
        val localvar = 1
        method(
            Block(InstrICONST(5), InstrISTORE(localvar), InstrLoadConfig("A"), InstrIFEQ(2)),
            Block(InstrICONST(1), InstrISTORE(localvar), InstrICONST(3), InstrDBGIPrint()),
            Block(InstrILOAD(localvar), InstrDBGIPrint(), InstrRETURN())
        )
    }



    test("plain loop") {
        val localvar = 1
        method(
            Block(InstrICONST(3), InstrISTORE(localvar)),
            Block(InstrILOAD(localvar), InstrDBGIPrint(), InstrIINC(localvar, -1), InstrILOAD(localvar), InstrIFEQ(3)),
            Block(InstrGOTO(1)),
            Block(InstrILOAD(localvar), InstrDBGIPrint(), InstrRETURN())
        )
    }

    test("cond loop bound") {
        val localvar = 1
        method(
            Block(InstrICONST(3), InstrLoadConfig("A"), InstrLoadConfig("B"), InstrIADD(), InstrIADD(), InstrISTORE(localvar)),
            Block(InstrILOAD(localvar), InstrDBGIPrint(), InstrIINC(localvar, -1), InstrILOAD(localvar), InstrIFEQ(3)),
            Block(InstrGOTO(1)),
            Block(InstrILOAD(localvar), InstrDBGIPrint(), InstrRETURN())
        )
    }

}
