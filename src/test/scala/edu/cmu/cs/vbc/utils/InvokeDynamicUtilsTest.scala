package edu.cmu.cs.vbc.utils

import edu.cmu.cs.vbc.vbytecode._
import edu.cmu.cs.vbc.vbytecode.instructions._
import edu.cmu.cs.vbc.{DiffMethodTestInfrastructure, InstrDBGIPrint, InstrDBGStrPrint, InstrLoadConfig}
import org.scalatest.FlatSpec

/**
  * @author chupanw
  */
class InvokeDynamicUtilsTest extends FlatSpec with DiffMethodTestInfrastructure {

  /** Helper function to create a variational Integer.
    *
    * @param startBlockIdx  the index of starting block, used to jump between blocks
    * @param tValue         int value if condition is true
    * @param fValue         int value if condition is false
    * @param localVar       store the new variational Integer to this local variable, otherwise leave it on stack
    * @return               A list of bytecode blocks
    */
  def createVInteger(startBlockIdx: Int, tValue: Int, fValue: Int, localVar: Option[LocalVar] = None): List[Block] = {
    Block(InstrLoadConfig("A"), InstrIFEQ(startBlockIdx + 2)) ::
    Block(
      InstrICONST(tValue),
      InstrINVOKESTATIC(Owner("java/lang/Integer"), MethodName("valueOf"), MethodDesc("(I)Ljava/lang/Integer;"), itf = false),
      if (localVar.isDefined) InstrASTORE(localVar.get) else InstrNOP(),
      InstrGOTO(startBlockIdx + 3)
    ) ::
    Block(
      InstrICONST(fValue),
      InstrINVOKESTATIC(Owner("java/lang/Integer"), MethodName("valueOf"), MethodDesc("(I)Ljava/lang/Integer;"), itf = false),
      if (localVar.isDefined) InstrASTORE(localVar.get) else InstrNOP(),
      // To handle unbalanced stack, current VBCAnalyzer requires the last instruction to be a jump instruction.
      InstrGOTO(startBlockIdx + 3)
    ) ::
    Nil
  }

  /** Create a variational Integer and call toString
    *
    * V object, no arguments
    */
  "InvokeDynamicUtils" can "invoke methods on V object without arguments" in {
    methodWithBlocks(
      createVInteger(0, tValue = 0, fValue = 1) :::
      Block(
        InstrINVOKEVIRTUAL(Owner("java/lang/Integer"), MethodName("toString"), MethodDesc("()Ljava/lang/String;"), itf = false),
        InstrDBGStrPrint(), InstrRETURN()
      ) ::
      Nil
    )
  }

  /** Create a variational Integer and a normal Integer, then call compareTo()
    *
    * V object, non-V argument
    */
  it can "invoke methods on V object with non-V argument" in {
    methodWithBlocks(
      createVInteger(0, tValue = 0, fValue = 2) :::
      Block(
        InstrICONST(1),
        InstrINVOKESTATIC(Owner("java/lang/Integer"), MethodName("valueOf"), MethodDesc("(I)Ljava/lang/Integer;"), itf = false),
        InstrINVOKEVIRTUAL(Owner("java/lang/Integer"), MethodName("compareTo"), MethodDesc("(Ljava/lang/Integer;)I"), itf = false),
        InstrDBGIPrint(), InstrRETURN()
      ) ::
      Nil
    )
  }

  /** Create two variational Integers and then call compareTo.
    *
    * V object, one V argument
    */
  it can "invoke methods on V object with one V argument" in {
    val v1 = new LocalVar("v1", "Ljava/lang/Integer;")
    val v2 = new LocalVar("v2", "Ljava/lang/Integer;")
    methodWithBlocks(
      createVInteger(0, tValue = 1, fValue = 3, localVar = Some(v1)) :::
      createVInteger(3, tValue = 0, fValue = 4, localVar = Some(v2)) :::
      Block(
        InstrALOAD(v1),
        InstrALOAD(v2),
        InstrINVOKEVIRTUAL(
          Owner("java/lang/Integer"),
          MethodName("compareTo"),
          MethodDesc("(Ljava/lang/Integer;)I"),
          itf = false
        ),
        InstrDBGIPrint(), InstrRETURN()
      ) ::
      Nil
    )
  }
}
