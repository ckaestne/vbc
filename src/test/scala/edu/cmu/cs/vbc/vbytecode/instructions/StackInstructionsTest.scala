package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.vbytecode.{Block, MethodDesc, MethodName, Owner}
import edu.cmu.cs.vbc.{DiffMethodTestInfrastructure, InstrDBGIPrint, InstrDBGStrPrint}
import org.scalatest.FlatSpec

/**
  * @author chupanw
  */
class StackInstructionsTest extends FlatSpec with DiffMethodTestInfrastructure {

  "DUP2" can "duplicate two ints" in {
    methodWithBlocks(
      Block(InstrICONST(1),
            InstrICONST(2),
            InstrDUP2(),
            InstrDBGIPrint(),
            InstrDBGIPrint(),
            InstrDBGIPrint(),
            InstrDBGIPrint(),
            InstrRETURN()) ::
        Nil
    )
  }

  it can "duplicate two vints" in {
    methodWithBlocks(
      createVint(tValue = 1, fValue = 2, startBlockIdx = 0, config = "A") :::
        createVint(tValue = 3, fValue = 4, startBlockIdx = 3, config = "B") :::
        Block(InstrDUP2(),
              InstrDBGIPrint(),
              InstrDBGIPrint(),
              InstrDBGIPrint(),
              InstrDBGIPrint(),
              InstrRETURN()) ::
        Nil
    )
  }

  it can "duplicate one long" in {
    methodWithBlocks(
      Block(
        InstrINVOKESTATIC(Owner.getRuntime,
                          MethodName("getRuntime"),
                          MethodDesc("()Ljava/lang/Runtime;"),
                          false),
        InstrINVOKEVIRTUAL(Owner.getRuntime,
                           MethodName("maxMemory"),
                           MethodDesc("()J"),
                           false),
        InstrDUP2(),
        InstrINVOKESTATIC(Owner.getLong,
                          MethodName("valueOf"),
                          MethodDesc("(J)Ljava/lang/Long;"),
                          false),
        InstrINVOKEVIRTUAL(Owner.getLong,
                           MethodName("toString"),
                           MethodDesc("()Ljava/lang/String;"),
                           false),
        InstrDBGStrPrint(),
        InstrINVOKESTATIC(Owner.getLong,
                          MethodName("valueOf"),
                          MethodDesc("(J)Ljava/lang/Long;"),
                          false),
        InstrINVOKEVIRTUAL(Owner.getLong,
                           MethodName("toString"),
                           MethodDesc("()Ljava/lang/String;"),
                           false),
        InstrDBGStrPrint(),
        InstrRETURN()
      ) ::
        Nil
    )
  }
}
