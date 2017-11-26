package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.OpcodePrint
import edu.cmu.cs.vbc.analysis.VBCFrame.UpdatedFrame
import edu.cmu.cs.vbc.analysis.{VBCFrame, V_TYPE}
import edu.cmu.cs.vbc.utils.LiftUtils
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{Label, MethodVisitor, Type}

trait Instruction {

  def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block)

  def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block)

  /**
    * Update the stack symbolically after executing this instruction
    *
    * @return UpdatedFrame is a tuple consisting of new VBCFrame and a backtrack instructions.
    *         If backtrack instruction set is not empty, we need to backtrack because we finally realise we need to lift
    *         that instruction. By default every backtracked instruction should be lifted, except for GETFIELD,
    *         PUTFIELD, INVOKEVIRTUAL, and INVOKESPECIAL, because lifting them or not depends on the type of object
    *         currently on stack. If the object is a V, we need to lift these instructions with INVOKEDYNAMIC.
    *
    *         If backtrack instruction set is not empty, the returned VBCFrame is useless, current frame will be pushed
    *         to queue again and reanalyze later. (see [[edu.cmu.cs.vbc.analysis.VBCAnalyzer.computeBeforeFrames]]
    */
  def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame

  def doBacktrack(env: VMethodEnv) = env.setLift(this)

  def getVariables: Set[LocalVar] = Set()

  final def isJumpInstr: Boolean = getJumpInstr.isDefined

  def getJumpInstr: Option[JumpInstruction] = None

  def isReturnInstr: Boolean = false

  def isATHROW: Boolean = false

  def isRETURN: Boolean = false


  /**
    * Used to identify the start of init method
    *
    * @see [[Rewrite.rewrite()]]
    */
  def isALOAD0: Boolean = false

  /**
    * Used to identify the start of init method
    *
    * @see [[Rewrite.rewrite()]]
    */
  def isINVOKESPECIAL_OBJECT_INIT: Boolean = false

  /**
    * instructions should not be compared for structural equality but for object identity.
    * overwriting case class defaults to original Java defaults
    */
  override def equals(that: Any) = that match {
    case t: AnyRef => t eq this
    case _ => false
  }
}


case class UNKNOWN(opCode: Int = -1) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
//    throw new RuntimeException("Unknown Instruction in " + s"${env.clazz.name}#${env.method.name}" + ": " + OpcodePrint.print(opCode))
    val err="Unknown Instruction in " + s"${env.clazz.name}#${env.method.name}" + ": " + OpcodePrint.print(opCode)
    System.err.println(err)
    mv.visitLdcInsn(err)
    mv.visitTypeInsn(NEW, "java/lang/RuntimeException")
    mv.visitInsn(DUP)
    mv.visitMethodInsn(INVOKESPECIAL, "java/lang/RuntimeException","<init>","(Ljava/lang/String;)V",false)
    mv.visitInsn(ATHROW)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    throw new RuntimeException("Unknown Instruction: " + OpcodePrint.print(opCode))
  }


  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame =
    throw new RuntimeException("Unknown Instruction: " + OpcodePrint.print(opCode) + s" in ${env.method.name} of ${env.clazz.name}")
}

trait EmptyInstruction extends Instruction

case class InstrNOP() extends EmptyInstruction {
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = toByteCode(mv, env, block)

  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(NOP)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = (s, Set.empty[Instruction])
}

case class InstrLINENUMBER(line: Int) extends EmptyInstruction {
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = toByteCode(mv, env, block)

  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    val l = new Label()
    mv.visitLabel(l)
    mv.visitLineNumber(line, l)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = (s, Set())
}


abstract class AbstractInstrINIT_FIELDS(isStatic: Boolean) extends Instruction {
  def PUTINSTR = if (isStatic) PUTSTATIC else PUTFIELD

  import FieldInitHelper._

  def initConditionalFieldValuePlain(mv: MethodVisitor, f: VBCFieldNode): Boolean

  def initConditionalFieldValueV(mv: MethodVisitor, env: VMethodEnv, block: Block, f: VBCFieldNode)

  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    if (isStatic) assert(env.method.name == "<clinit>")
    else assert(env.method.name == "<init>")
    val fields = env.clazz.fields.filter(f => f.isStatic == isStatic)

    // no nothing for normal fields (they are initialized the usual way)

    // assign the config value for @Conditional fields if they are configured
    for (f <- fields; if f.hasConditionalAnnotation()) {
      if (initConditionalFieldValuePlain(mv, f)) {
        if (!isStatic) {
          mv.visitVarInsn(ALOAD, 0)
          mv.visitInsn(SWAP)
        }
        mv.visitFieldInsn(PUTINSTR, env.clazz.name, f.name, f.desc)
      }
    }
  }


  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    //must be called from within constructor/clinit
    if (isStatic)
      assert(env.method.name == "<clinit>")
    else assert(env.method.name == "<init>")

    val fields = env.clazz.fields.filter(f => f.isStatic == isStatic)

    // init all fields with @Conditional annotations according to config parameter
    for (f <- fields; if f.hasConditionalAnnotation()) {
      if (!isStatic) mv.visitVarInsn(ALOAD, 0)
      initConditionalFieldValueV(mv, env, block, f)
      mv.visitFieldInsn(PUTINSTR, env.clazz.name, f.name, "Ledu/cmu/cs/varex/V;")
    }

    // initialize all other fields by creating One's for their original values
    for (f <- fields; if !f.hasConditionalAnnotation()) {
      if (!isStatic) mv.visitVarInsn(ALOAD, 0)
      createOne(f, mv, env, block)
      mv.visitFieldInsn(PUTINSTR, env.clazz.name, f.name, "Ledu/cmu/cs/varex/V;")
    }
  }


  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = (s, Set())

}

/**
  * Helper instruction for initializing fields after lifting
  * Also sets config values in both lifted and unlifted code
  *
  * Fields are no longer initialized directly (because they are objects now), but with
  * this instruction in the constructor/<clinit> (the latter for static fields).
  *
  * config parameter describes the initial value (true or false, or None for variational)
  *
  * should only occur in the method ___clinit___
  *
  * TODO: initialization of nonstatic @Conditional might be surpressed if not initialized
  * to conditional value? check.
  */
case class InstrINIT_FIELDS(isStatic: Boolean, config: String => Option[Boolean]) extends AbstractInstrINIT_FIELDS(isStatic) {
  override def initConditionalFieldValuePlain(mv: MethodVisitor, f: VBCFieldNode): Boolean = {
    val init = config(f.name)
    if (init != None) {
      mv.visitInsn(if (init == Some(true)) ICONST_1 else ICONST_0)
      true
    } else false
  }

  import FieldInitHelper._

  override def initConditionalFieldValueV(mv: MethodVisitor, env: VMethodEnv, block: Block, f: VBCFieldNode): Unit = {
    val c = config(f.name)
    if (c == Some(true))
      createConstOne(ICONST_1, mv, env, block)
    else if (c == Some(false))
      createConstOne(ICONST_0, mv, env, block)
    else
      createChoice(f.name, mv, env, block)
  }
}

object FieldInitHelper {

  import LiftUtils._


  def createConstOne(const: Int, mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    mv.visitInsn(const)
    mv.visitMethodInsn(INVOKESTATIC, Owner.getInt, "valueOf", s"(I)${Owner.getInt.getTypeDesc}", false)
    callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
  }

  def createChoice(fName: String, mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    mv.visitLdcInsn(fName)
    mv.visitMethodInsn(INVOKESTATIC, fexprfactoryClassName, "createDefinedExternal", "(Ljava/lang/String;)Lde/fosd/typechef/featureexpr/SingleFeatureExpr;", false)
    createConstOne(ICONST_1, mv, env, block)
    createConstOne(ICONST_0, mv, env, block)
    callVCreateChoice(mv)
  }

  def createOne(f: VBCFieldNode, mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    Type.getType(f.desc).getSort match {
      case Type.INT =>
        if (f.value == null) mv.visitInsn(ICONST_0) else pushConstant(mv, f.value.asInstanceOf[Int])
        mv.visitMethodInsn(INVOKESTATIC, Owner.getInt, "valueOf", s"(I)${Owner.getInt.getTypeDesc}", false)
      case Type.OBJECT => mv.visitInsn(ACONST_NULL)
      case Type.SHORT =>
        if (f.value == null) mv.visitInsn(ICONST_0) else pushConstant(mv, f.value.asInstanceOf[Int])
        mv.visitMethodInsn(INVOKESTATIC, Owner.getInt, "valueOf", s"(I)${Owner.getInt.getTypeDesc}", false)
      case Type.BYTE =>
        if (f.value == null) mv.visitInsn(ICONST_0) else pushConstant(mv, f.value.asInstanceOf[Int])
        mv.visitMethodInsn(INVOKESTATIC, Owner.getInt, "valueOf", s"(I)${Owner.getInt.getTypeDesc}", false)
      case Type.CHAR =>
        if (f.value == null) mv.visitInsn(ICONST_0) else pushConstant(mv, f.value.asInstanceOf[Int])
        mv.visitMethodInsn(INVOKESTATIC, Owner.getInt, "valueOf", s"(I)${Owner.getInt.getTypeDesc}", false)
      case Type.BOOLEAN =>
        if (f.value == null) mv.visitInsn(ICONST_0) else pushConstant(mv, f.value.asInstanceOf[Int])
        mv.visitMethodInsn(INVOKESTATIC, Owner.getInt, "valueOf", s"(I)${Owner.getInt.getTypeDesc}", false)
      case Type.LONG =>
        if (f.value == null) mv.visitInsn(LCONST_0) else pushLongConstant(mv, f.value.asInstanceOf[Long])
        mv.visitMethodInsn(INVOKESTATIC, Owner.getLong, "valueOf", s"(J)${Owner.getLong.getTypeDesc}", false)
      case Type.FLOAT =>
        if (f.value == null) mv.visitInsn(FCONST_0) else pushFloatConstant(mv, f.value.asInstanceOf[Float])
        float2Float(mv)
      case Type.ARRAY => mv.visitInsn(ACONST_NULL)
      case _ =>
        ???
    }
    callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
  }
}

case class InstrStartTimer(id: String) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {}

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    mv.visitLdcInsn(id)
    mv.visitMethodInsn(INVOKESTATIC, Owner("edu/cmu/cs/vbc/utils/Profiler"), MethodName("startTimer"), MethodDesc("(Ljava/lang/String;)V"), false)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = (s, Set())
}

case class InstrStopTimer(id: String) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {}

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    mv.visitLdcInsn(id)
    mv.visitMethodInsn(INVOKESTATIC, Owner("edu/cmu/cs/vbc/utils/Profiler"), MethodName("stopTimer"), MethodDesc("(Ljava/lang/String;)V"), false)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = (s, Set())
}

/**
  * Our own instruction for wrapping the value on stack into V.One
  *
  * This is used, for example, in our fake TryCatchBlocks to wrap the exceptions.
  */
case class InstrWrapOne() extends Instruction {

  import LiftUtils._

  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {} // do nothing
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit =
    callVCreateOne(mv, loadCurrentCtx(_, env, block))

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = {
    val (_, _, frame) = s.pop()
    (frame.push(V_TYPE(false), Set(this)), Set())
  }
}
