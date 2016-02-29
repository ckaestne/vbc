package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.OpcodePrint
import edu.cmu.cs.vbc.vbytecode._
import edu.cmu.cs.vbc.vbytecode.util.LiftUtils
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

trait Instruction extends LiftUtils {
    def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block)

    def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block)

    def getVariables: Set[LocalVar] = Set()

    def getJumpInstr: Option[JumpInstruction] = None

    final def isJumpInstr: Boolean = getJumpInstr.isDefined

    def isReturnInstr: Boolean = false
}


case class UNKNOWN(opCode: Int = -1) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        throw new RuntimeException("Unknown Instruction: " + OpcodePrint.print(opCode))
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        throw new RuntimeException("Unknown Instruction: " + OpcodePrint.print(opCode))
    }
}


case class InstrNOP() extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        mv.visitInsn(NOP)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    }
}


/**
  * Helper instruciton for initializing conditional fields
  */
case class InstrINIT_CONDITIONAL_FIELDS() extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        // do nothing
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        for (conditionalField <- env.clazz.fields
             if conditionalField.hasConditionalAnnotation) {
            mv.visitVarInsn(ALOAD, 0)
            mv.visitLdcInsn(conditionalField.name)
            mv.visitMethodInsn(INVOKESTATIC, fexprfactoryClassName, "createDefinedExternal", "(Ljava/lang/String;)Lde/fosd/typechef/featureexpr/SingleFeatureExpr;", false)
            mv.visitInsn(ICONST_1)
            mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
            callVCreateOne(mv)
            mv.visitInsn(ICONST_0)
            mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
            callVCreateOne(mv)
            callVCreateChoice(mv)
            mv.visitFieldInsn(PUTFIELD, env.clazz.name, conditionalField.name, "Ledu/cmu/cs/varex/V;")
        }
    }
}
