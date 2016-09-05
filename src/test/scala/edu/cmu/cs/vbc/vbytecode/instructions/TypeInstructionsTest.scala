package edu.cmu.cs.vbc.vbytecode.instructions

import java.lang.reflect.InvocationTargetException

import edu.cmu.cs.vbc.vbytecode.{MethodDesc, MethodName, Owner}
import edu.cmu.cs.vbc.{DiffMethodTestInfrastructure, InstrDBGStrPrint}
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

  ignore should "throw exception if checking primitive array type fails" in {
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

  ignore should "throw exception if checking object array type fails" in {
    val caught = intercept[InvocationTargetException] {
      simpleMethod(
        InstrBIPUSH(10), InstrANEWARRAY(Owner("java/lang/Integer")),
        InstrCHECKCAST(Owner("[Ljava/lang/Integer;")),
        InstrPOP(), InstrRETURN()
      )
    }
    assert(caught.getCause.getClass == classOf[ClassCastException], "Expecting ClassCastException")
  }
}
