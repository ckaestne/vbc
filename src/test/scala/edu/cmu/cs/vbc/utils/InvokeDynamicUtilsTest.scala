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

  /** Helper function to create a variational String. */
  def createVString(startBlockIdx: Int, tValue: String, fValue: String, localVar: Option[LocalVar] = None, config: String = "A"): List[Block] = {
    Block(InstrLoadConfig(config), InstrIFEQ(startBlockIdx + 2)) ::
      Block(
        InstrLDC(tValue),
        if (localVar.isDefined) InstrASTORE(localVar.get) else InstrNOP(),
        InstrGOTO(startBlockIdx + 3)
      ) ::
      Block(
        InstrLDC(fValue),
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
  "InvokeDynamicUtils" can "invoke method on V object without arguments" in {
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
  it can "invoke method on V object with non-V argument" in {
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
  it can "invoke method on V object with one V argument" in {
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

  /** Create one variational String, and then call replaceAll()
    *
    * V object, two V arguments.
    */
  it can "invoke method on V object with two V arguments" in {
    val obj = new LocalVar("obj", "Ljava/lang/String;")
    val regex = new LocalVar("regex", "Ljava/lang/String;")
    val rep = new LocalVar("rep", "Ljava/lang/String;")
    methodWithBlocks(
      createVString(0, tValue = "apple and peach are friends", fValue = "apple and pear are friends", Some(obj)) :::
        createVString(3, tValue = "friends", fValue = "are", Some(regex)) :::
        createVString(6, tValue = "enemies", fValue = "are not", Some(rep), config = "B") :::
        Block(
          InstrALOAD(obj), InstrALOAD(regex), InstrALOAD(rep),
          InstrINVOKEVIRTUAL(
            Owner("java/lang/String"),
            MethodName("replaceAll"),
            MethodDesc("(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;"),
            itf = false
          ),
          InstrDBGStrPrint(), InstrRETURN()
        ) ::
        Nil
    )
  }

  /** Create one normal Integer, and then call toString()
    *
    * non-V object, no arguments
    */
  it can "invoke method on non-V object without arguments" in {
    methodWithBlocks(
      Block(
        InstrICONST(1),
        InstrINVOKESTATIC(Owner("java/lang/Integer"), MethodName("valueOf"), MethodDesc("(I)Ljava/lang/Integer;"), itf = false),
        InstrINVOKEVIRTUAL(Owner("java/lang/Integer"), MethodName("toString"), MethodDesc("()Ljava/lang/String;"), itf = false),
        InstrDBGStrPrint(), InstrRETURN()
      ) ::
        Nil
    )
  }

  /** Create three normal String, and then call replaceAll()
    *
    * non-V object, two non-V arguments
    */
  it can "invoke method on non-V object with non-V arguments" in {
    methodWithBlocks(
      Block(
        InstrLDC("apple is delicious"), InstrLDC("is"), InstrLDC("is not"),
        InstrINVOKEVIRTUAL(
          Owner("java/lang/String"),
          MethodName("replaceAll"),
          MethodDesc("(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;"),
          itf = false
        ),
        InstrDBGStrPrint(), InstrRETURN()
      ) :: Nil
    )
  }

  /** Create one normal Integer and one variational Integer, then call compareTo
    *
    * non-V object, one V arguments
    */
  it can "invoke method on non-V object with one V argument" in {
    val b = new LocalVar("b", TypeDesc("Ljava/lang/Integer;"))
    methodWithBlocks(
      createVInteger(0, tValue = 0, fValue = 2, Some(b)) :::
        Block(
          InstrICONST(1),
          InstrINVOKESTATIC(Owner("java/lang/Integer"), MethodName("valueOf"), MethodDesc("(I)Ljava/lang/Integer;"), itf = false),
          InstrALOAD(b),
          InstrINVOKEVIRTUAL(Owner("java/lang/Integer"), MethodName("compareTo"), MethodDesc("(Ljava/lang/Integer;)I"), itf = false),
          InstrDBGIPrint(), InstrRETURN()
        ) ::
        Nil
    )
  }

  /** Create one normal String and two variational Strings, then call replaceAll
    *
    * non-V object, two V arguments
    */
  it can "invoke method on non-V object with two V arguments" in {
    val a = new LocalVar("a", TypeDesc("Ljava/lang/String;"))
    val b = new LocalVar("b", TypeDesc("Ljava/lang/String;"))
    methodWithBlocks(
      createVString(0, tValue = "pear", fValue = "are", Some(a), config = "A") :::
        createVString(3, tValue = "peach", fValue = "aren't", Some(b), config = "B") :::
        Block(
          InstrLDC("apple and pear are friends"),
          InstrALOAD(a), InstrALOAD(b),
          InstrINVOKEVIRTUAL(
            Owner("java/lang/String"),
            MethodName("replaceAll"),
            MethodDesc("(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;"),
            itf = false
          ),
          InstrDBGStrPrint(), InstrRETURN()
        ) ::
        Nil
    )
  }
}
