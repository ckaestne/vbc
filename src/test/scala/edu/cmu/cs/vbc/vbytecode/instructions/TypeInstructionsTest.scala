package edu.cmu.cs.vbc.vbytecode.instructions

import java.lang.reflect.InvocationTargetException

import edu.cmu.cs.vbc.vbytecode._
import edu.cmu.cs.vbc.{DiffMethodTestInfrastructure, InstrDBGIPrint, InstrDBGStrPrint, InstrLoadConfig}
import org.objectweb.asm.Opcodes._
import org.scalatest.FlatSpec

/** Test suite for type instructions.
  *
  * Including: CHECKCAST, I2C, NEW
  *
  * @author chupanw
  */
class TypeInstructionsTest extends FlatSpec with DiffMethodTestInfrastructure {

  /** CHECKCAST test suite
    *
    * @todo Add test cases for multidimensional arrays
    */
  "CHECKCAST" can "check object type" in {
    simpleMethod(
      InstrICONST(1),
      InstrINVOKESTATIC(Owner("java/lang/Integer"), MethodName("valueOf"), MethodDesc("(I)Ljava/lang/Integer;"), itf = false),
      InstrCHECKCAST(Owner("java/lang/Integer")),
      InstrINVOKEVIRTUAL(Owner("java/lang/Integer"), MethodName("toString"), MethodDesc("()Ljava/lang/String;"), itf = false),
      InstrDBGStrPrint(),
      InstrRETURN()
    )
  }

  it should "throw exception if checking object type fails" in {
    val caught = intercept[InvocationTargetException] {
      simpleMethod(
        InstrBIPUSH(1), InstrINVOKESTATIC(Owner("java/lang/Integer"), MethodName("valueOf"), MethodDesc("(I)Ljava/lang/Integer;"), itf = false),
        InstrCHECKCAST(Owner("java/lang/Boolean")),
        InstrPOP(), InstrRETURN()
      )
    }
    assert(caught.getCause.getClass == classOf[ClassCastException], "Expecting ClassCastException")
  }

  it can "check primitive array type" in {
    simpleMethod(
      InstrBIPUSH(10), InstrNEWARRAY(T_INT),
      InstrCHECKCAST(Owner("[I")),
      InstrPOP(), InstrRETURN()
    )
  }

  it should "throw exception if checking primitive array type fails" in {
    val caught = intercept[InvocationTargetException] {
      simpleMethod(
        InstrBIPUSH(10), InstrNEWARRAY(T_INT),
        InstrCHECKCAST(Owner("[B")),
        InstrPOP(), InstrRETURN()
      )
    }
    assert(caught.getCause.getClass == classOf[ClassCastException], "Expecting ClassCastException")
  }


  it can "check object array" in {
    simpleMethod(
      InstrBIPUSH(10), InstrANEWARRAY(Owner("java/lang/Integer")),
      InstrCHECKCAST(Owner("[Ljava/lang/Integer;")),
      InstrPOP(), InstrRETURN()
    )
  }

  it should "throw exception if checking object array type fails" in {
    val caught = intercept[InvocationTargetException] {
      simpleMethod(
        InstrBIPUSH(10), InstrANEWARRAY(Owner("java/lang/Integer")),
        InstrCHECKCAST(Owner("[Ljava/lang/Boolean;")),
        InstrPOP(), InstrRETURN()
      )
    }
    assert(caught.getCause.getClass == classOf[ClassCastException], "Expecting ClassCastException")
  }

  "INSTANCEOF" can "check non-null object" in {
    methodWithBlocks(List(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(InstrLDC("hello"), InstrGOTO(3)),
      Block(
        InstrICONST(1),
        InstrINVOKESTATIC(Owner.getInt, MethodName("valueOf"), MethodDesc(s"(I)${TypeDesc.getInt}"), itf = false),
        InstrGOTO(3)
      ),
      Block(InstrINSTANCEOF(Owner.getInt), InstrDBGIPrint(), InstrRETURN())
    ))
  }
}
