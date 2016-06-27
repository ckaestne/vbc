package edu.cmu.cs.vbc

import de.fosd.typechef.featureexpr.FeatureExprFactory
import edu.cmu.cs.vbc.test.{InstrDBGIPrint, InstrDBGStrPrint, InstrLoadConfig}
import edu.cmu.cs.vbc.vbytecode._
import edu.cmu.cs.vbc.vbytecode.instructions.{InstrATHROW, _}
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Opcodes.ACC_PUBLIC
import org.scalatest.FunSuite


class VBCExceptionTest extends FunSuite with DiffMethodTestInfrastructure {

  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)

  private def runMethodI(instrs: List[Instruction], otherMethods: List[VBCMethodNode] = Nil) =
    runMethod(List(Block(instrs: _*)), otherMethods)

  private def runMethod(blocks: List[Block], otherMethods: List[VBCMethodNode] = Nil) =
    testMethod(new VBCMethodNode(ACC_PUBLIC, "test", "()V", Some("()V"), Nil,
      CFG(splitBlocksOnMethods(blocks))), otherMethods: _*)

  private def method(name: String, desc: String, body: Block*) =
    new VBCMethodNode(0, name, desc, None, Nil, CFG(body.toList))

  private def methodI(name: String, desc: String, instrs: Instruction*) =
    new VBCMethodNode(0, name, desc, None, Nil, CFG(List(Block(instrs.toList: _*))))

  def invokeVirt(m: VBCMethodNode) = InstrINVOKEVIRTUAL("Test", m.name, m.desc, false)

  val thisvar = new Parameter(0, "this")

  def createException(classname: String, msg: String): List[Instruction] =
    InstrNEW(classname) :: InstrDUP() :: InstrLDC(msg) :: InstrINVOKESPECIAL(classname, "<init>", "(Ljava/lang/String;)V", false) :: Nil


  test("terminate with exception") {
    runMethod(
      List(Block(createException("java/lang/Exception", "foo") :+ InstrATHROW(): _*))
    )
  }

  test("conditionally terminate with exception") {
    runMethod(List(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(createException("java/lang/Exception", "foo") :+ InstrATHROW(): _*),
      Block(InstrICONST(4), InstrDBGIPrint(), InstrRETURNVoid())
    ))
  }

  test("conditionally terminate with different exception") {
    runMethod(List(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(createException("java/lang/Exception", "foo") :+ InstrATHROW(): _*),
      Block(InstrLoadConfig("B"), InstrIFEQ(4)),
      Block(createException("java/lang/Exception", "bar") :+ InstrATHROW(): _*),
      Block(InstrICONST(4), InstrDBGIPrint(), InstrRETURNVoid())
    ))
  }

  test("terminate with alternative exception") {
    runMethod(List(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(createException("java/lang/Exception", "foo") :+ InstrATHROW(): _*),
      Block(createException("java/lang/Exception", "bar") :+ InstrATHROW(): _*)
    ))
  }

  test("terminate with alternative exception on stack") {
    runMethod(List(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(createException("java/lang/Exception", "foo") :+ InstrGOTO(3): _*),
      Block(createException("java/lang/Exception", "bar"): _*),
      Block(InstrATHROW())
    ))
  }

  test("terminate with alternative exception in var") {
    val exVar = new LocalVar("ex", "Ljava/lang/Exception;")
    runMethod(List(
      Block(createException("java/lang/Exception", "foo") :+ InstrASTORE(exVar): _*),
      Block(InstrLoadConfig("A"), InstrIFEQ(3)),
      Block(createException("java/lang/Exception", "bar") :+ InstrASTORE(exVar): _*),
      Block(InstrLoadConfig("B"), InstrIFEQ(5)),
      Block(InstrALOAD(exVar), InstrATHROW()),
      Block(InstrICONST(4), InstrDBGIPrint(), InstrRETURNVoid())
    ))
  }

  test("terminate with exception from atomic instruction") {
    //TODO why is this all lifted, there is no inner variability? -- shouldn't tagV handle this? @chupanw
    runMethod(List(
      Block(InstrICONST(0), InstrICONST(0), InstrIDIV(), InstrDBGIPrint(), InstrRETURNVoid())
    ))
  }

  test("conditionally terminate with exception from atomic instruction") {
    runMethod(List(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(InstrICONST(0), InstrICONST(0), InstrIDIV(), InstrDBGIPrint(), InstrRETURNVoid()),
      Block(InstrICONST(0), InstrICONST(1), InstrIDIV(), InstrDBGIPrint(), InstrRETURNVoid())
    ))
  }

  test("conditionally terminate with exception from atomic instruction 2") {
    runMethod(List(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(InstrICONST(0), InstrICONST(1), InstrIDIV(), InstrDBGIPrint(), InstrRETURNVoid()),
      Block(InstrICONST(0), InstrICONST(0), InstrIDIV(), InstrDBGIPrint(), InstrRETURNVoid())
    ))
  }

  test("caught atomic exception") {
    runMethod(List(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(InstrICONST(1), InstrDBGIPrint(), InstrICONST(0), InstrICONST(1), InstrIDIV(), InstrDBGIPrint(), InstrRETURNVoid()),
      Block(Seq(InstrICONST(2), InstrDBGIPrint(), InstrICONST(0), InstrICONST(0), InstrIDIV(), InstrDBGIPrint(), InstrRETURNVoid()), Seq(VBCHandler("java/lang/ArithmeticException", 3))),
      Block(InstrPOP(), InstrICONST(-1), InstrDBGIPrint(), InstrRETURNVoid())
    ))
  }

  test("two blocks with same exception handler") {
    runMethod(List(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(Seq(InstrICONST(1), InstrDBGIPrint(), InstrICONST(0), InstrICONST(0), InstrIDIV(), InstrDBGIPrint(), InstrRETURNVoid()), Seq(VBCHandler("java/lang/ArithmeticException", 3))),
      Block(Seq(InstrICONST(2), InstrDBGIPrint(), InstrICONST(0), InstrICONST(0), InstrIDIV(), InstrDBGIPrint(), InstrRETURNVoid()), Seq(VBCHandler("java/lang/ArithmeticException", 3))),
      Block(InstrPOP(), InstrICONST(-1), InstrDBGIPrint(), InstrRETURNVoid())
    ))
  }

  test("jump directly to exception handler") {
    runMethod(List(
      Block(InstrLoadConfig("A"), InstrIFEQ(2)),
      Block(InstrICONST(1) +: InstrDBGIPrint() +: createException("java/lang/Exception", "foo") :+ InstrGOTO(3): _*),
      Block(Seq(InstrICONST(2), InstrDBGIPrint(), InstrICONST(0), InstrICONST(0), InstrIDIV(), InstrDBGIPrint(), InstrRETURNVoid()), Seq(VBCHandler("java/lang/ArithmeticException", 3))),
      Block(InstrPOP(), InstrICONST(-1), InstrDBGIPrint(), InstrRETURNVoid())
    ))
  }

  test("caught atomic exception in one branch") {
    runMethod(List(
      Block(InstrLoadConfig("A"), InstrIFEQ(3)),
      Block(InstrLoadConfig("B"), InstrIFEQ(4)),
      Block(InstrICONST(1), InstrDBGIPrint(), InstrICONST(0), InstrICONST(1), InstrIDIV(), InstrDBGIPrint(), InstrRETURNVoid()),
      Block(Seq(InstrICONST(2), InstrDBGIPrint(), InstrICONST(0), InstrICONST(0), InstrIDIV(), InstrDBGIPrint(), InstrRETURNVoid()), Seq(VBCHandler("java/lang/ArithmeticException", 5))),
      Block(InstrICONST(2), InstrDBGIPrint(), InstrICONST(0), InstrICONST(0), InstrIDIV(), InstrDBGIPrint(), InstrRETURNVoid()),
      Block(InstrPOP(), InstrICONST(-1), InstrDBGIPrint(), InstrRETURNVoid())
    ))
  }

  ignore("partial exception on IDIV on variational values") {
    //TODO does not yet support partial exceptions in IDIV library function
    val x = new LocalVar("x", "I")
    runMethod(List(
      /*0*/ Block(InstrLoadConfig("A"), InstrIFEQ(2)),
      /*1*/ Block(InstrICONST(1), InstrISTORE(x), InstrGOTO(3)),
      /*2*/ Block(InstrICONST(0), InstrISTORE(x)),
      /*3*/ Block(InstrICONST(5), InstrILOAD(x), InstrIDIV(), InstrDBGIPrint(), InstrRETURNVoid())
    ))
  }

  // returns a choice between 0 and an ArithmeticException depending on A
  val m_partialException = method("partialException", "()I",
    Block(InstrLoadConfig("A"), InstrIFEQ(2)),
    Block(InstrICONST(0), InstrICONST(1), InstrIDIV(), InstrRETURNVal(Opcodes.IRETURN)),
    Block(InstrICONST(0), InstrICONST(0), InstrIDIV(), InstrRETURNVal(Opcodes.IRETURN))
  )

  // throws a VException with two different possible exceptions
  val m_vException = method("vException", "()I",
    Block(InstrLoadConfig("A"), InstrIFEQ(3)),
    Block(createException("java/lang/Exception", "foo"): _*),
    Block(InstrATHROW()),
    Block(createException("java/lang/RuntimeException", "bar"): _*),
    Block(InstrATHROW())
  )

  val m_vException2 = method("vException2", "()I",
    Block(InstrLoadConfig("A"), InstrIFEQ(6)),
    Block(InstrLoadConfig("C"), InstrIFEQ(4)),
    Block(createException("java/lang/RuntimeException", "foo"): _*),
    Block(InstrATHROW()),
    Block(createException("java/lang/InterruptedException", "interr"): _*),
    Block(InstrATHROW()),
    Block(InstrLoadConfig("B"), InstrIFEQ(9)),
    Block(createException("java/lang/NullPointerException", "nullp"): _*),
    Block(InstrATHROW()),
    Block(createException("java/lang/Exception", "bar"): _*),
    Block(InstrATHROW())
  )

  test("correctly propagate VException") {
    runMethod(List(
      Block(InstrALOAD(thisvar), invokeVirt(m_vException), InstrDBGIPrint(), InstrRETURNVoid())
    ), List(m_vException))
  }

  test("partially catch VException") {
    runMethod(List(
      Block(List(InstrALOAD(thisvar), invokeVirt(m_vException), InstrDBGIPrint(), InstrRETURNVoid()), Seq(VBCHandler("java/lang/RuntimeException", 1))),
      Block(InstrPOP(), InstrLDC("caught runtime exception"), InstrDBGStrPrint(), InstrRETURNVoid())
    ), List(m_vException))
  }

  test("partially catch VException and print exception message") {
    runMethod(List(
      Block(List(InstrALOAD(thisvar), invokeVirt(m_vException), InstrDBGIPrint(), InstrRETURNVoid()), Seq(VBCHandler("java/lang/RuntimeException", 1))),
      Block(InstrINVOKEVIRTUAL("java/lang/Exception", "getMessage", "()Ljava/lang/String;", false), InstrDBGStrPrint(), InstrRETURNVoid())
    ), List(m_vException))
  }

  test("catch all VException alternatives and print exception messages") {
    runMethod(List(
      Block(List(InstrALOAD(thisvar), invokeVirt(m_vException), InstrDBGIPrint(), InstrRETURNVoid()), Seq(VBCHandler("java/lang/Throwable", 1))),
      Block(InstrINVOKEVIRTUAL("java/lang/Throwable", "getMessage", "()Ljava/lang/String;", false), InstrDBGStrPrint(), InstrRETURNVoid())
    ), List(m_vException))
  }


  test("catch all VException alternatives and print exception messages 2") {
    runMethod(List(
      Block(List(InstrALOAD(thisvar), invokeVirt(m_vException2), InstrDBGIPrint(), InstrRETURNVoid()), Seq(VBCHandler("java/lang/RuntimeException", 1))),
      Block(InstrINVOKEVIRTUAL("java/lang/Throwable", "getMessage", "()Ljava/lang/String;", false), InstrDBGStrPrint(), InstrRETURNVoid())
    ), List(m_vException2))
  }

  test("partially continue after partial exception from method call") {
    runMethod(List(
      Block(InstrALOAD(thisvar), invokeVirt(m_partialException), InstrDBGIPrint(), InstrRETURNVoid())
    ), List(m_partialException))
  }


}
