package edu.cmu.cs.vbc.vbytecode

import de.fosd.typechef.featureexpr.FeatureExprFactory
import edu.cmu.cs.vbc
import edu.cmu.cs.vbc.vbytecode.instructions._
import edu.cmu.cs.vbc.{DiffMethodTestInfrastructure, InstrLoadConfig}
import org.objectweb.asm.Opcodes.ACC_PUBLIC
import org.scalatest.FunSuite


class VBCControlFlowTest extends FunSuite with DiffMethodTestInfrastructure {

  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)

  private def method(blocks: Block*) =
    testMethod(new VBCMethodNode(ACC_PUBLIC, "test", "()V", Some("()V"), Nil,
      CFG(blocks.toList)))

  test("basic if-then") {
    method(
      Block(InstrICONST(0), InstrIFEQ(2)),
      Block(InstrICONST(3), vbc.InstrDBGIPrint()),
      Block(InstrICONST(4), vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }
  test("basic if-else") {
    method(
      Block(InstrICONST(1), InstrIFEQ(2)),
      Block(InstrICONST(3), vbc.InstrDBGIPrint()),
      Block(InstrICONST(4), vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }
  test("conditional if") {
    method(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(InstrICONST(3), vbc.InstrDBGIPrint()),
      Block(InstrICONST(4), vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }

  test("nested conditional if") {
    method(
      Block(InstrLoadConfig("A"), InstrIFEQ(3)),
      Block(InstrLoadConfig("B"), InstrIFEQ(3)),
      Block(InstrICONST(3), vbc.InstrDBGIPrint()),
      Block(InstrICONST(4), vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }

  test("nested conditional if2") {
    method(
      Block(vbc.InstrDBGCtx("L0"), InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(vbc.InstrDBGCtx("L1"), InstrLoadConfig("B"), InstrIFEQ(3)),
      Block(vbc.InstrDBGCtx("L2"), InstrICONST(3), vbc.InstrDBGIPrint(), InstrGOTO(4)),
      Block(vbc.InstrDBGCtx("L3"), InstrICONST(5), vbc.InstrDBGIPrint()),
      Block(vbc.InstrDBGCtx("L4"), InstrICONST(4), vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }

  test("conditional store") {
    val localvar = new LocalVar("v", "I")
    method(
      Block(InstrICONST(5), InstrISTORE(localvar), InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(InstrICONST(1), InstrISTORE(localvar), InstrICONST(3), vbc.InstrDBGIPrint()),
      Block(InstrILOAD(localvar), vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }



  test("plain loop") {
    val localvar = new LocalVar("v", "I")
    method(
      Block(InstrICONST(3), InstrISTORE(localvar)),
      Block(InstrILOAD(localvar), vbc.InstrDBGIPrint(), InstrIINC(localvar, -1), InstrILOAD(localvar), InstrIFEQ(3)),
      Block(InstrGOTO(1)),
      Block(InstrILOAD(localvar), vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }

  test("cond loop bound") {
    val localvar = new LocalVar("v", "I")
    method(
      Block(InstrICONST(3), InstrLoadConfig("A"), InstrLoadConfig("B"), InstrIADD(), InstrIADD(), InstrISTORE(localvar)),
      Block(InstrILOAD(localvar), vbc.InstrDBGIPrint(), InstrIINC(localvar, -1), InstrILOAD(localvar), InstrIFEQ(3)),
      Block(InstrGOTO(1)),
      Block(InstrILOAD(localvar), vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }

  test("two returns") {
    method(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(InstrICONST(3), vbc.InstrDBGIPrint(), InstrRETURN()),
      Block(InstrICONST(4), vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }

  test("IFNE") {
    method(
      Block(InstrLoadConfig("A"), InstrIFNE(2)),
      Block(InstrICONST(1), vbc.InstrDBGIPrint()),
      Block(InstrICONST(2), vbc.InstrDBGIPrint()),
      Block(InstrRETURN())
    )
  }

  test("unbalanced - 1 producer 2 consumer") {
    method(
      Block(InstrICONST(1), InstrLoadConfig("A"), InstrIFNE(2)),
      Block(vbc.InstrDBGIPrint(), InstrGOTO(3)),
      Block(vbc.InstrDBGIPrint(), InstrGOTO(3)),
      Block(InstrICONST(0), vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }

  test("unbalanced - 2 producer 1 comsumer") {
    method(
      Block(InstrLoadConfig("A"), InstrIFNE(2)),
      Block(InstrICONST(1), InstrGOTO(3)),
      Block(InstrICONST(2), InstrGOTO(3)),
      Block(vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }

  test("unbalanced - 1 producer 2 consumer with 1 transit each") {
    method(
      Block(InstrICONST(1), InstrLoadConfig("A"), InstrIFNE(3)),
      Block(InstrICONST(2), vbc.InstrDBGIPrint(), InstrGOTO(2)),
      Block(vbc.InstrDBGIPrint(), InstrGOTO(5)),
      Block(InstrICONST(3), vbc.InstrDBGIPrint(), InstrGOTO(4)),
      Block(vbc.InstrDBGIPrint(), InstrGOTO(5)),
      Block(InstrICONST(0), vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }

  test("unbalanced - 2 producer with 1 transit each 1 consumer") {
    method(
      Block(InstrLoadConfig("A"), InstrIFNE(3)),
      Block(InstrICONST(1), InstrGOTO(2)),
      Block(InstrICONST(3), vbc.InstrDBGIPrint(), InstrGOTO(5)),
      Block(InstrICONST(2), InstrGOTO(4)),
      Block(InstrICONST(4), vbc.InstrDBGIPrint(), InstrGOTO(5)),
      Block(vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }

  test("unbalanced - for loop") {
    val local = new LocalVar("v", "I")
    method(
      Block(InstrICONST(10), InstrISTORE(local), InstrICONST(1), InstrGOTO(1)),
      Block(InstrICONST(2), InstrICONST(3), InstrILOAD(local), InstrICONST(0), InstrIF_ICMPGE(3)),
      Block(vbc.InstrDBGIPrint(), InstrGOTO(4)),
      Block(InstrPOP(), InstrPOP(), InstrILOAD(local), InstrICONST(1), InstrISUB(), InstrISTORE(local), InstrICONST(-1), vbc.InstrDBGIPrint(), InstrGOTO(1)),
      Block(vbc.InstrDBGIPrint(), vbc.InstrDBGIPrint(), InstrICONST(0), vbc.InstrDBGIPrint(), InstrRETURN())
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
      Block(vbc.InstrDBGIPrint(), InstrGOTO(7)),
      Block(InstrPOP(), InstrPOP(), InstrILOAD(local), InstrICONST(1), InstrISUB(), InstrISTORE(local), InstrICONST(-1), vbc.InstrDBGIPrint(), InstrGOTO(4)),
      Block(vbc.InstrDBGIPrint(), vbc.InstrDBGIPrint(), InstrICONST(0), vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }

  test("redundant jump") {
    method(
      Block(InstrLoadConfig("A"), InstrIFNE(1)),
      Block(InstrRETURN())
    )
  }

  test("unbalanced non-V value") {
    method(
      Block(InstrNEW("java/lang/Integer"), InstrDUP(), InstrICONST(3), InstrINVOKESPECIAL(Owner("java/lang/Integer"), MethodName("<init>"), MethodDesc("(I)V"), false)),
      Block(InstrINVOKEVIRTUAL(Owner("java/lang/Integer"), MethodName("toString"), MethodDesc("()Ljava/lang/String;"), false)),
      Block(vbc.InstrDBGStrPrint()),
      Block(InstrRETURN())
    )
  }

  test("unbalanced non-V value with GOTOs") {
    method(
      Block(InstrNEW("java/lang/Integer"), InstrDUP(), InstrICONST(3), InstrINVOKESPECIAL(Owner("java/lang/Integer"), MethodName("<init>"), MethodDesc("(I)V"), false), InstrGOTO(1)),
      Block(InstrINVOKEVIRTUAL(Owner("java/lang/Integer"), MethodName("toString"), MethodDesc("()Ljava/lang/String;"), false), InstrGOTO(2)),
      Block(vbc.InstrDBGStrPrint(), InstrRETURN())
    )
  }

  test("IFNONNULL") {
    val variable = new LocalVar("v", "R")
    method(
      Block(InstrICONST(1), InstrINVOKESTATIC(Owner("java/lang/Integer"), MethodName("valueOf"), MethodDesc("(I)Ljava/lang/Integer;"), itf = false), InstrASTORE(variable)),
      Block(InstrLoadConfig("A"), InstrIFGE(3)),
      Block(InstrACONST_NULL(), InstrGOTO(4)),
      Block(InstrALOAD(variable), InstrGOTO(4)),
      Block(InstrIFNONNULL(6)),
      Block(InstrLDC("null"), vbc.InstrDBGStrPrint(), InstrGOTO(7)),
      Block(InstrLDC("nonnull"), vbc.InstrDBGStrPrint(), InstrGOTO(7)),
      Block(InstrRETURN())
    )
  }

  test("IFLT") {
    method(
      Block(InstrLoadConfig("A"), InstrICONST(1), InstrISUB(), InstrIFLT(2)),
      Block(InstrICONST(1), InstrGOTO(3)),
      Block(InstrICONST(2), InstrGOTO(3)),
      Block(InstrICONST(3), InstrIADD(), vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }

  test("IF_ICMPLE") {
    method(
      Block(InstrLoadConfig("A"), InstrICONST(0), InstrIF_ICMPLE(2)),
      Block(InstrICONST(1), InstrGOTO(3)),
      Block(InstrICONST(2), InstrGOTO(3)),
      Block(InstrICONST(3), InstrIADD(), vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }

  ignore("NEW reference left on stack") {
    method(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(InstrNEW("java/lang/Integer"), InstrDUP(), InstrINVOKESPECIAL(Owner("java/lang/Integer"), MethodName("<init>"), MethodDesc("()V"), itf = false), InstrGOTO(3)),
      Block(InstrNEW("java/lang/Integer")),
      Block(InstrPOP(), InstrRETURN())
    )
  }

  test("I2C without information loss") {
    method(
      Block(InstrICONST(1), InstrLoadConfig("A"), InstrI2C(), InstrIF_ICMPEQ(2)),
      Block(InstrICONST(1), InstrGOTO(3)),
      Block(InstrICONST(2)),
      Block(vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }

  test("I2C with information loss") {
    val twoToSeventeen: java.lang.Integer = 131072
    method(
      // 131072 = 2^17, since char in JVM is 16-bit, stack value becomes 0 after I2C
      Block(InstrLDC(twoToSeventeen), InstrI2C(), InstrLoadConfig("A"), InstrIF_ICMPEQ(2)),
      Block(InstrICONST(1), InstrGOTO(3)),
      Block(InstrICONST(2)),
      Block(vbc.InstrDBGIPrint(), InstrRETURN())
    )
  }
}
