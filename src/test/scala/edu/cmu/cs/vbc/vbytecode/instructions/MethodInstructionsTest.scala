package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.vbytecode._
import edu.cmu.cs.vbc.{DiffMethodTestInfrastructure, InstrDBGIPrint, InstrDBGStrPrint}
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
}
