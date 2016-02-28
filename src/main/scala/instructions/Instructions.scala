package edu.cmu.cs.vbc.instructions

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

/**
  * IF* and GOTO instructions
  */
trait JumpInstruction extends Instruction {
    /**
      * gets the successor of a jump. the first value is the
      * target of an unconditional jump, but it can be None
      * when it just falls through to the next block
      * the second value is the target of a conditional jump,
      * if any
      */
    def getSuccessor(): (Option[Int], Option[Int])

    override def getJumpInstr: Option[JumpInstruction] = Some(this)
}


case class InstrIADD() extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        mv.visitInsn(IADD)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "IADD", "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Ledu/cmu/cs/varex/V;", false)
    }
}


case class InstrDUP() extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        mv.visitInsn(DUP)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        //TODO, when applied to LONG, use the int one instead of the 2byte one
        mv.visitInsn(DUP)
    }
}

case class InstrICONST(v: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        pushConstant(mv, v)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        pushConstant(mv, v)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
        callVCreateOne(mv)
    }
}


case class InstrRETURN() extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
        mv.visitInsn(RETURN)

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit =
        mv.visitInsn(RETURN)

    override def isReturnInstr: Boolean = true

}


case class InstrIINC(variable: LocalVar, increment: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
        mv.visitIincInsn(env.getVarIdx(variable), increment)

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        loadV(mv, env, variable)
        pushConstant(mv, increment)
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "IINC", "(Ledu/cmu/cs/varex/V;I)Ledu/cmu/cs/varex/V;", false)

        //create a choice with the original value
        loadFExpr(mv, env, env.getBlockVar(block))
        mv.visitInsn(SWAP)
        loadV(mv, env, variable)
        callVCreateChoice(mv)

        storeV(mv, env, variable)
    }

    override def getVariables() = Set(variable)
}

case class InstrISTORE(variable: LocalVar) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
        mv.visitVarInsn(ISTORE, env.getVarIdx(variable))

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        //TODO is it worth optimizing this in case ctx is TRUE (or the initial method's ctx)?

        //new value is already on top of stack
        loadFExpr(mv, env, env.getBlockVar(block))
        mv.visitInsn(SWAP)
        loadV(mv, env, variable)
        //now ctx, newvalue, oldvalue on stack
        callVCreateChoice(mv)
        //now new choice value on stack combining old and new value
        storeV(mv, env, variable)
    }

    override def getVariables() = Set(variable)
}

case class InstrILOAD(variable: LocalVar) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
        mv.visitVarInsn(ILOAD, env.getVarIdx(variable))

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        loadV(mv, env, variable)
    }

    override def getVariables() = Set(variable)
}


/**
  * Helper instruciton for initializing conditional fields
  */
case class InstrINIT_CONDITIONAL_FIELDS() extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        // do nothing
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        //    for (i <- FieldTransformer.fields) {
        //        mv.visitVarInsn(ALOAD, 0)
        //        mv.visitLdcInsn(name)
        //        mv.visitMethodInsn(INVOKESTATIC, fexprFactoryClassName, "createDefinedExternal", "(Ljava/lang/String;)Lde/fosd/typechef/featureexpr/SingleFeatureExpr;", false)
        //        mv.visitInsn(ICONST_1)
        //        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
        //        callVCreateOne(mv)
        //        mv.visitInsn(ICONST_0)
        //        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
        //        callVCreateOne(mv)
        //        callVCreateChoice(mv)
        //        mv.visitFieldInsn(PUTFIELD, owner, name, "Ledu/cmu/cs/varex/V;")
        //    }
    }
}


