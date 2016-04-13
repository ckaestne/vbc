package edu.cmu.cs.vbc

import de.fosd.typechef.featureexpr.FeatureExprFactory
import edu.cmu.cs.vbc.test.{InstrDBGCtx, InstrDBGIPrint, InstrDBGStrPrint, InstrLoadConfig}
import edu.cmu.cs.vbc.vbytecode._
import edu.cmu.cs.vbc.vbytecode.instructions._
import org.objectweb.asm.Opcodes.ACC_PUBLIC
import org.scalatest.FunSuite


class VBCControlFlowTest extends FunSuite with DiffMethodTestInfrastructure {

  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)

  private def method(blocks: Block*) =
    testMethod(new VBCMethodNode(ACC_PUBLIC, "test", "()V", Some("()V"), Nil,
      CFG(splitBlocksOnMethods(blocks.toList))))

  test("basic if-then") {
    method(
      Block(InstrICONST(0), InstrIFEQ(2)),
      Block(InstrICONST(3), InstrDBGIPrint()),
      Block(InstrICONST(4), InstrDBGIPrint(), InstrRETURNVoid())
    )
  }
  test("basic if-else") {
    method(
      Block(InstrICONST(1), InstrIFEQ(2)),
      Block(InstrICONST(3), InstrDBGIPrint()),
      Block(InstrICONST(4), InstrDBGIPrint(), InstrRETURNVoid())
    )
  }
  test("conditional if") {
    method(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(InstrICONST(3), InstrDBGIPrint()),
      Block(InstrICONST(4), InstrDBGIPrint(), InstrRETURNVoid())
    )
  }

  test("nested conditional if") {
    method(
      Block(InstrLoadConfig("A"), InstrIFEQ(3)),
      Block(InstrLoadConfig("B"), InstrIFEQ(3)),
      Block(InstrICONST(3), InstrDBGIPrint()),
      Block(InstrICONST(4), InstrDBGIPrint(), InstrRETURNVoid())
    )
  }

  test("nested conditional if2") {
    method(
      Block(InstrDBGCtx("L0"), InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(InstrDBGCtx("L1"), InstrLoadConfig("B"), InstrIFEQ(3)),
      Block(InstrDBGCtx("L2"), InstrICONST(3), InstrDBGIPrint(), InstrGOTO(4)),
      Block(InstrDBGCtx("L3"), InstrICONST(5), InstrDBGIPrint()),
      Block(InstrDBGCtx("L4"), InstrICONST(4), InstrDBGIPrint(), InstrRETURNVoid())
    )
  }

  test("conditional store") {
    val localvar = new LocalVar("v", "I")
    method(
      Block(InstrICONST(5), InstrISTORE(localvar), InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(InstrICONST(1), InstrISTORE(localvar), InstrICONST(3), InstrDBGIPrint()),
      Block(InstrILOAD(localvar), InstrDBGIPrint(), InstrRETURNVoid())
    )
  }



  test("plain loop") {
    val localvar = new LocalVar("v", "I")
    method(
      Block(InstrICONST(3), InstrISTORE(localvar)),
      Block(InstrILOAD(localvar), InstrDBGIPrint(), InstrIINC(localvar, -1), InstrILOAD(localvar), InstrIFEQ(3)),
      Block(InstrGOTO(1)),
      Block(InstrILOAD(localvar), InstrDBGIPrint(), InstrRETURNVoid())
    )
  }

  test("cond loop bound") {
    val localvar = new LocalVar("v", "I")
    method(
      Block(InstrICONST(3), InstrLoadConfig("A"), InstrLoadConfig("B"), InstrIADD(), InstrIADD(), InstrISTORE(localvar)),
      Block(InstrILOAD(localvar), InstrDBGIPrint(), InstrIINC(localvar, -1), InstrILOAD(localvar), InstrIFEQ(3)),
      Block(InstrGOTO(1)),
      Block(InstrILOAD(localvar), InstrDBGIPrint(), InstrRETURNVoid())
    )
  }

  test("two returns") {
    method(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(InstrICONST(3), InstrDBGIPrint(), InstrRETURNVoid()),
      Block(InstrICONST(4), InstrDBGIPrint(), InstrRETURNVoid())
    )
  }

  test("IFNE") {
    method(
      Block(InstrLoadConfig("A"), InstrIFNE(2)),
      Block(InstrICONST(1), InstrDBGIPrint()),
      Block(InstrICONST(2), InstrDBGIPrint()),
      Block(InstrRETURNVoid())
    )
  }

  test("unbalanced - 1 producer 2 consumer") {
    method(
      Block(InstrICONST(1), InstrLoadConfig("A"), InstrIFNE(2)),
      Block(InstrDBGIPrint(), InstrGOTO(3)),
      Block(InstrDBGIPrint(), InstrGOTO(3)),
      Block(InstrICONST(0), InstrDBGIPrint(), InstrRETURNVoid())
    )
  }

  test("unbalanced - 2 producer 1 comsumer") {
    method(
      Block(InstrLoadConfig("A"), InstrIFNE(2)),
      Block(InstrICONST(1), InstrGOTO(3)),
      Block(InstrICONST(2), InstrGOTO(3)),
      Block(InstrDBGIPrint(), InstrRETURNVoid())
    )
  }

  test("unbalanced - 1 producer 2 consumer with 1 transit each") {
    method(
      Block(InstrICONST(1), InstrLoadConfig("A"), InstrIFNE(3)),
      Block(InstrICONST(2), InstrDBGIPrint(), InstrGOTO(2)),
      Block(InstrDBGIPrint(), InstrGOTO(5)),
      Block(InstrICONST(3), InstrDBGIPrint(), InstrGOTO(4)),
      Block(InstrDBGIPrint(), InstrGOTO(5)),
      Block(InstrICONST(0), InstrDBGIPrint(), InstrRETURNVoid())
    )
  }

  test("unbalanced - 2 producer with 1 transit each 1 consumer") {
    method(
      Block(InstrLoadConfig("A"), InstrIFNE(3)),
      Block(InstrICONST(1), InstrGOTO(2)),
      Block(InstrICONST(3), InstrDBGIPrint(), InstrGOTO(5)),
      Block(InstrICONST(2), InstrGOTO(4)),
      Block(InstrICONST(4), InstrDBGIPrint(), InstrGOTO(5)),
      Block(InstrDBGIPrint(), InstrRETURNVoid())
    )
  }

  test("unbalanced - for loop") {
    val local = new LocalVar("v", "I")
    method(
      Block(InstrICONST(10), InstrISTORE(local), InstrICONST(1), InstrGOTO(1)),
      Block(InstrICONST(2), InstrICONST(3), InstrILOAD(local), InstrICONST(0), InstrIF_ICMPGE(3)),
      Block(InstrDBGIPrint(), InstrGOTO(4)),
      Block(InstrPOP(), InstrPOP(), InstrILOAD(local), InstrICONST(1), InstrISUB(), InstrISTORE(local), InstrICONST(-1), InstrDBGIPrint(), InstrGOTO(1)),
      Block(InstrDBGIPrint(), InstrDBGIPrint(), InstrICONST(0), InstrDBGIPrint(), InstrRETURNVoid())
    )
  }


  test("unbalanced - conditional for loop") {
    val local = new LocalVar("v", "I")
    method(
      Block(InstrLoadConfig("A"), InstrIFNE(2)),
      Block(InstrICONST(5), InstrISTORE(local), InstrGOTO(3)),
      Block(InstrICONST(10), InstrISTORE(local), InstrGOTO(3)),
      Block(InstrICONST(1), InstrGOTO(4)),
      Block(InstrICONST(2), InstrICONST(3), InstrILOAD(local), InstrICONST(0), InstrIF_ICMPGE(6)),
      Block(InstrDBGIPrint(), InstrGOTO(7)),
      Block(InstrPOP(), InstrPOP(), InstrILOAD(local), InstrICONST(1), InstrISUB(), InstrISTORE(local), InstrICONST(-1), InstrDBGIPrint(), InstrGOTO(4)),
      Block(InstrDBGIPrint(), InstrDBGIPrint(), InstrICONST(0), InstrDBGIPrint(), InstrRETURNVoid())
    )
  }

  test("redundant jump") {
    method(
      Block(InstrLoadConfig("A"), InstrIFNE(1)),
      Block(InstrRETURNVoid())
    )
  }

  test("unbalanced non-V value") {
    method(
      Block(InstrNEW("java/lang/Integer"), InstrDUP(), InstrICONST(3), InstrINVOKESPECIAL("java/lang/Integer", "<init>", "(I)V", false)),
      Block(InstrINVOKEVIRTUAL("java/lang/Integer", "toString", "()Ljava/lang/String;", false)),
      Block(InstrDBGStrPrint()),
      Block(InstrRETURNVoid())
    )
  }
}
