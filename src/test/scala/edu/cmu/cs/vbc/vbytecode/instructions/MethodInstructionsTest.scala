package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.vbytecode._
import edu.cmu.cs.vbc.{DiffMethodTestInfrastructure, InstrDBGIPrint, InstrDBGStrPrint, InstrLoadConfig}
import org.scalatest.FlatSpec

/**
  * @author chupanw
  */
class MethodInstructionsTest extends FlatSpec with DiffMethodTestInfrastructure {

  "INVOKESTATIC" can "invoke unlifted method with one primitive argument" in {
    val a = new LocalVar("a", TypeDesc.getInt)
    val b = new LocalVar("b", TypeDesc.getInt)
    methodWithBlocks(
      createVInteger(0, tValue = 0, fValue = 2, Some(a), config = "A") :::
        createVInteger(3, tValue = 1, fValue = 3, Some(b), config = "B") :::
        Block(
          InstrALOAD(a), InstrALOAD(b),
          InstrINVOKEVIRTUAL(
            Owner("java/lang/Integer"),
            MethodName("compareTo"),
            MethodDesc("(Ljava/lang/Integer;)I"),
            false
          ),
          InstrINVOKESTATIC(
            Owner("java/lang/Integer"),
            MethodName("valueOf"),
            MethodDesc("(I)Ljava/lang/Integer;"),
            itf = false
          ),
          InstrINVOKEVIRTUAL(
            Owner("java/lang/Integer"),
            MethodName("toString"),
            MethodDesc("()Ljava/lang/String;"),
            itf = false
          ),
          InstrDBGStrPrint(), InstrRETURN()
        ) ::
        Nil
    )
  }

  it can "invoke unlifted method with two primitive arguments" in {
    val a = new LocalVar("a", TypeDesc.getInt)
    val b = new LocalVar("b", TypeDesc.getInt)
    methodWithBlocks(
      createVInteger(0, tValue = 0, fValue = 2, Some(a), config = "A") :::
        createVInteger(3, tValue = 1, fValue = 3, Some(b), config = "B") :::
        Block(
          InstrALOAD(a), InstrINVOKEVIRTUAL(Owner.getInt, MethodName("intValue"), MethodDesc("()I"), itf = false),
          InstrALOAD(b), InstrINVOKEVIRTUAL(Owner.getInt, MethodName("intValue"), MethodDesc("()I"), itf = false),
          InstrINVOKESTATIC(Owner.getInt, MethodName("max"), MethodDesc("(II)I"), itf = false),
          InstrDBGIPrint(), InstrRETURN()
        ) ::
        Nil
    )
  }

  it can "invoke method that takes an integer argument" in {
    methodWithBlocks(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)) ::
        Block(InstrICONST(1), InstrGOTO(3)) ::
        Block(InstrICONST(2), InstrGOTO(3)) ::
        Block(
          InstrINVOKESTATIC(Owner.getString, MethodName("valueOf"), MethodDesc("(I)Ljava/lang/String;"), itf = false),
          InstrDBGStrPrint(), InstrRETURN()
        ) ::
        Nil
    )
  }

  it can "invoke method that takes a boolean argument" in {
    methodWithBlocks(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)) ::
        Block(InstrICONST(0), InstrGOTO(3)) ::
        Block(InstrICONST(1), InstrGOTO(3)) ::
        Block(
          InstrINVOKESTATIC(Owner.getString, MethodName("valueOf"), MethodDesc("(Z)Ljava/lang/String;"), itf = false),
          InstrDBGStrPrint(), InstrRETURN()
        ) ::
        Nil
    )
  }

  it can "invoke method that takes a char argument" in {
    methodWithBlocks(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)) ::
        Block(InstrICONST(0), InstrGOTO(3)) ::
        Block(InstrICONST(1), InstrGOTO(3)) ::
        Block(
          InstrINVOKESTATIC(Owner.getString, MethodName("valueOf"), MethodDesc("(C)Ljava/lang/String;"), itf = false),
          InstrDBGStrPrint(), InstrRETURN()
        ) ::
        Nil
    )
  }

  it can "invoke method that takes a short argument" in {
    methodWithBlocks(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)) ::
        Block(InstrICONST(0), InstrGOTO(3)) ::
        Block(InstrICONST(1), InstrGOTO(3)) ::
        Block(
          InstrINVOKESTATIC(Owner.getShort, MethodName("toString"), MethodDesc("(S)Ljava/lang/String;"), itf = false),
          InstrDBGStrPrint(), InstrRETURN()
        ) ::
        Nil
    )
  }

  it can "invoke method that takes a byte argument" in {
    methodWithBlocks(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)) ::
        Block(InstrICONST(0), InstrGOTO(3)) ::
        Block(InstrICONST(1), InstrGOTO(3)) ::
        Block(
          InstrINVOKESTATIC(Owner.getByte, MethodName("toString"), MethodDesc("(B)Ljava/lang/String;"), itf = false),
          InstrDBGStrPrint(), InstrRETURN()
        ) ::
        Nil
    )
  }

  "INVOKEVIRTUAL" can "invoke unlifed method with one primitive argument" in {
    val a = new LocalVar("a", TypeDesc.getString)
    val b = new LocalVar("b", TypeDesc.getInt)
    methodWithBlocks(
      createVString(0, tValue = "hello world", fValue = "hi world", Some(a), config = "A") :::
        createVInteger(3, tValue = 1, fValue = 3, Some(b), config = "B") :::
        Block(
          InstrALOAD(a),
          InstrALOAD(b), InstrINVOKEVIRTUAL(Owner.getInt, MethodName("intValue"), MethodDesc("()I"), itf = false),
          InstrINVOKEVIRTUAL(Owner.getString, MethodName("codePointAt"), MethodDesc("(I)I"), itf = false),
          InstrDBGIPrint(), InstrRETURN()
        ) ::
        Nil
    )
  }

  it can "invoke unlifted method with two primitive arguments" in {
    val str = new LocalVar("str", TypeDesc.getString)
    val begin = new LocalVar("begin", TypeDesc.getInt)
    val end = new LocalVar("end", TypeDesc.getInt)
    methodWithBlocks(
      createVString(0, tValue = "abcdefghijklmn", fValue = "opqrstuvwxyz", Some(str), config = "A") :::
        createVInteger(3, tValue = 0, fValue = 2, Some(begin), config = "A") :::
        createVInteger(6, tValue = 6, fValue = 8, Some(end), config = "B") :::
        Block(
          InstrALOAD(str),
          InstrALOAD(begin), InstrINVOKEVIRTUAL(Owner.getInt, MethodName("intValue"), MethodDesc("()I"), itf = false),
          InstrALOAD(end), InstrINVOKEVIRTUAL(Owner.getInt, MethodName("intValue"), MethodDesc("()I"), itf = false),
          InstrINVOKEVIRTUAL(Owner.getString, MethodName("substring"), MethodDesc("(II)Ljava/lang/String;"), itf = false),
          InstrDBGStrPrint(), InstrRETURN()
        ) ::
        Nil
    )
  }

  it can "invoke method that takes a boolean argument" in {
    methodWithBlocks(
      Block(
        InstrGETSTATIC(Owner("java/lang/System"), FieldName("out"), TypeDesc("Ljava/io/PrintStream;")),
        InstrLoadConfig("A"), InstrIFEQ(2)
      ) ::
        Block(InstrICONST(0), InstrGOTO(3)) ::
        Block(InstrICONST(1), InstrGOTO(3)) ::
        Block(
          InstrINVOKEVIRTUAL(Owner("java/io/PrintStream"), MethodName("println"), MethodDesc("(Z)V"), itf = false),
          InstrRETURN()
        ) ::
        Nil
    )
  }

  it can "invoke method that takes a char argument" in {
    methodWithBlocks(
      Block(
        InstrGETSTATIC(Owner("java/lang/System"), FieldName("out"), TypeDesc("Ljava/io/PrintStream;")),
        InstrLoadConfig("A"), InstrIFEQ(2)
      ) ::
        Block(InstrICONST(0), InstrGOTO(3)) ::
        Block(InstrICONST(1), InstrGOTO(3)) ::
        Block(
          InstrINVOKEVIRTUAL(Owner("java/io/PrintStream"), MethodName("println"), MethodDesc("(C)V"), itf = false),
          InstrRETURN()
        ) ::
        Nil
    )
  }
}
