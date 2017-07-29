package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.vbytecode._
import edu.cmu.cs.vbc.{DiffMethodTestInfrastructure, InstrDBGIPrint, InstrDBGStrPrint}
import org.scalatest.FlatSpec

/**
  * @author chupanw
  */
class ArithmeticInstructionsTest extends FlatSpec with DiffMethodTestInfrastructure {
  "ISHL" can "shift left int value" in {
    methodWithBlocks(
      createVint(tValue = 1, fValue = 2, 0) :::
      Block(InstrICONST(1), InstrISHL(), InstrDBGIPrint(), InstrRETURN()) ::
      Nil
    )
  }

  "LCMP" can "compare VLong(equal and bigger) with long" in {
    methodWithBlocks(
      createVlong(tValue = 1, fValue = 2, startBlockIdx = 0) :::
      Block(InstrLDC(new java.lang.Long(1)), InstrLCMP(), InstrDBGIPrint(), InstrRETURN()) ::
      Nil
    )
  }

  it can "compare VLong(smaller and bigger) with long" in {
    methodWithBlocks(
      createVlong(tValue = 1, fValue = 3, startBlockIdx = 0) :::
      Block(InstrLDC(new java.lang.Long(2)), InstrLCMP(), InstrDBGIPrint(), InstrRETURN()) ::
      Nil
    )
  }

  it can "compare VLong with VLong" in {
    methodWithBlocks(
      createVlong(tValue = 1, fValue = 3, startBlockIdx = 0, config = "A") :::
      createVlong(tValue = 0, fValue = 2, startBlockIdx = 3, config = "B")  :::
      Block(InstrLCMP(), InstrDBGIPrint(), InstrRETURN()) ::
      Nil
    )
  }

  it should "throw exception if values being compared are not long" in {
    assertThrows[Throwable] {
      methodWithBlocks(
        createVlong(tValue = 1, fValue = 2, startBlockIdx = 0) :::
        Block(InstrICONST(1), InstrLCMP(), InstrDBGIPrint(), InstrRETURN()) ::
        Nil
      )
    }
  }

  "LNEG" can "negate long value" in {
    methodWithBlocks(
      createVlong(tValue = 1, fValue = 2, startBlockIdx = 0) :::
      Block(
        InstrLNEG(),
        InstrINVOKESTATIC(Owner.getString, MethodName("valueOf"), MethodDesc(s"(J)${TypeDesc.getString}"), itf = false),
        InstrDBGStrPrint(),
        InstrRETURN()
      ) ::
      Nil
    )
  }

  "ISHR" can "shift right int" in {
    methodWithBlocks(
      createVint(tValue = 1, fValue = 2, startBlockIdx = 0) :::
      createVint(tValue = 3, fValue = 4, startBlockIdx = 3) :::
      Block(InstrISHR(), InstrDBGIPrint(), InstrRETURN()) ::
      Nil
    )
  }

  "IUSHR" can "logical shift right int" in {
    methodWithBlocks(
      createVint(tValue = 1, fValue = 2, startBlockIdx = 0) :::
      createVint(tValue = 3, fValue = 4, startBlockIdx = 3) :::
      Block(InstrIUSHR(), InstrDBGIPrint(), InstrRETURN()) ::
      Nil
    )
  }

  it can "subtract numbers" in {
    methodWithBlocks(
      createVint(tValue = 1, fValue = 2, startBlockIdx = 0) :::
        createVint(tValue = 3, fValue = 4, startBlockIdx = 3) :::
        Block(InstrISUB(), InstrDBGIPrint(), InstrRETURN()) ::
        Nil
    )
  }

  "IAND" can "compute boolean AND between two Vints" in {
    methodWithBlocks(
      createVint(tValue = 1, fValue = 2, startBlockIdx = 0, config = "A") :::
      createVint(tValue = 3, fValue = 4, startBlockIdx = 3, config = "B") :::
      Block(InstrIAND(), InstrDBGIPrint(), InstrRETURN()) ::
      Nil
    )
  }

  it can "compute boolean AND between int and Vint" in {
    methodWithBlocks(
      createVint(tValue = 1, fValue = 2, startBlockIdx = 0, config = "A") :::
      Block(InstrICONST(3), InstrIAND(), InstrDBGIPrint(), InstrRETURN()) ::
      Nil
    )
  }

  "IXOR" can "compute exclusive or between two Vints" in {
    methodWithBlocks(
      createVint(tValue = 1, fValue = 2, startBlockIdx = 0, config = "A") :::
        createVint(tValue = 3, fValue = 4, startBlockIdx = 3, config = "B") :::
        Block(InstrIXOR(), InstrDBGIPrint(), InstrRETURN()) ::
        Nil
    )
  }

  it can "compute exclusive or between int and Vint" in {
    methodWithBlocks(
      createVint(tValue = 1, fValue = 2, startBlockIdx = 0, config = "A") :::
        Block(InstrICONST(3), InstrIXOR(), InstrDBGIPrint(), InstrRETURN()) ::
        Nil
    )
  }

  "LDIV" can "compute division between two Long values" in {
    methodWithBlocks(
      Block(
        InstrINVOKESTATIC(Owner.getRuntime, MethodName("getRuntime"), MethodDesc("()Ljava/lang/Runtime;"), false),
        InstrINVOKEVIRTUAL(Owner.getRuntime, MethodName("maxMemory"), MethodDesc("()J"), false),
        InstrINVOKESTATIC(Owner.getRuntime, MethodName("getRuntime"), MethodDesc("()Ljava/lang/Runtime;"), false),
        InstrINVOKEVIRTUAL(Owner.getRuntime, MethodName("totalMemory"), MethodDesc("()J"), false),
        InstrLDIV(),
        InstrINVOKESTATIC(Owner.getLong, MethodName("valueOf"), MethodDesc("(J)Ljava/lang/Long;"), false),
        InstrINVOKEVIRTUAL(Owner.getLong, MethodName("toString"), MethodDesc("()Ljava/lang/String;"), false),
        InstrDBGStrPrint(),
        InstrRETURN()
      ) ::
        Nil
    )
  }
}
