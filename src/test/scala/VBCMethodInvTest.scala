package edu.cmu.cs.vbc

import de.fosd.typechef.featureexpr.FeatureExprFactory
import edu.cmu.cs.vbc.test.{InstrDBGIPrint, InstrLoadConfig}
import edu.cmu.cs.vbc.vbytecode.instructions._
import edu.cmu.cs.vbc.vbytecode.{Block, _}
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Opcodes.ACC_PUBLIC
import org.scalatest.FunSuite


class VBCMethodInvTest extends FunSuite with DiffMethodTestInfrastructure {

  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)

  def InstrIRETURN() = InstrRETURNVal(Opcodes.IRETURN)


  private def runMethodI(instrs: List[Instruction], otherMethods: List[VBCMethodNode] = Nil) =
    runMethod(List(Block(instrs: _*)), otherMethods)

  private def runMethod(blocks: List[Block], otherMethods: List[VBCMethodNode] = Nil) =
    testMethod(new VBCMethodNode(ACC_PUBLIC, "test", "()V", Some("()V"), Nil,
      CFG(splitBlocksOnMethods(blocks))), otherMethods: _*)

  private def method(name: String, desc: String, body: Block*) =
    new VBCMethodNode(0, name, desc, None, Nil, CFG(body.toList))

  private def methodI(name: String, desc: String, instrs: Instruction*) =
    new VBCMethodNode(0, name, desc, None, Nil, CFG(List(Block(instrs.toList: _*))))


  val thisvar = new Parameter(0, "this")
  val param1 = new Parameter(1, "i")
  val testclass = "Test"
  val m_print0 = methodI("print0", "()V", InstrICONST(0), InstrDBGIPrint(), InstrRETURNVoid())
  val m_printI = methodI("printI", "(I)V", InstrILOAD(new Parameter(1, "i")), InstrDBGIPrint(), InstrRETURNVoid())
  val m_add1 = methodI("add1", "(I)I", InstrIINC(param1, 1), InstrILOAD(param1), InstrDUP(), InstrDBGIPrint(), InstrIRETURN())

  def createException(classname: String, msg: String): List[Instruction] =
    InstrNEW(classname) :: InstrDUP() :: InstrLDC(msg) :: InstrINVOKESPECIAL(classname, "<init>", "(Ljava/lang/String;)V", false) :: Nil

  def invokeVirt(m: VBCMethodNode) = InstrINVOKEVIRTUAL(testclass, m.name, m.desc, false)

  test("test without extra method") {
    runMethodI(List(
      InstrICONST(0), InstrDBGIPrint(), InstrRETURNVoid()
    ))
  }

  test("simple method call") {
    runMethod(
      List(Block(InstrALOAD(thisvar), invokeVirt(m_print0), InstrRETURNVoid())),
      List(m_print0)
    )
  }

  test("simple method call with param") {
    runMethod(
      List(Block(InstrALOAD(thisvar), InstrICONST(5), invokeVirt(m_printI), InstrRETURNVoid())),
      List(m_printI)
    )
  }

  test("simple method call with variational param") {
    runMethod(
      List(Block(InstrALOAD(thisvar), InstrLoadConfig("A"), invokeVirt(m_printI), InstrRETURNVoid())),
      List(m_printI)
    )
  }

  test("simple method call with variational result") {
    runMethod(
      List(Block(InstrALOAD(thisvar), InstrDUP(), InstrLoadConfig("A"), invokeVirt(m_add1), invokeVirt(m_printI), InstrRETURNVoid())),
      List(m_add1, m_printI)
    )
  }

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


}
