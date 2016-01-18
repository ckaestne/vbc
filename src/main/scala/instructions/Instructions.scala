package edu.cmu.cs.vbc.instructions

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._


trait Instruction extends LiftUtils {
    def toByteCode(mv: MethodVisitor, method: MethodNode, block: Block)

    def toVByteCode(mv: MethodVisitor, method: MethodNode, block: Block)

    def getVariables: Set[Integer] = Set()
}

case class InstrIADD() extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit = {
        mv.visitInsn(IADD)
    }

    override def toVByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit = {
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "IADD", "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Ledu/cmu/cs/varex/V;", false)
    }
}


case class InstrDUP() extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit = {
        mv.visitInsn(DUP)
    }

    override def toVByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit = {
        //TODO, when applied to LONG, use the int one instead of the 2byte one
        mv.visitInsn(DUP)
    }
}

case class InstrICONST(v: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit = {
        writeConstant(mv, v)
    }

    override def toVByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit = {
        writeConstant(mv, v)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
        writeVCreateOne(mv)
    }
}


case class InstrRETURN() extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit =
        mv.visitInsn(RETURN)

    override def toVByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit =
        mv.visitInsn(RETURN)
}



case class InstrIINC(variable: Int, increment: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit =
        mv.visitIincInsn(variable, increment)

    override def toVByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit = {
        mv.visitVarInsn(ALOAD, variable + ctxParameterOffset)
        writeConstant(mv, increment)
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "IINC", "(Ledu/cmu/cs/varex/V;I)Ledu/cmu/cs/varex/V;", false)

        //create a choice with the original value
        mv.visitVarInsn(ALOAD, block.blockConditionVar)
        mv.visitInsn(SWAP)
        mv.visitVarInsn(ALOAD, variable + ctxParameterOffset)
        writeVCreateChoice(mv)

        mv.visitVarInsn(ASTORE, variable + ctxParameterOffset)
    }

    override def getVariables() = Set(variable + ctxParameterOffset)
}

case class InstrISTORE(variable: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit =
        mv.visitVarInsn(ISTORE, variable)

    override def toVByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit = {
        //TODO is it worth optimizing this in case ctx is TRUE (or the initial method's ctx)?

        //new value is already on top of stack
        mv.visitVarInsn(ALOAD, block.blockConditionVar)
        mv.visitInsn(SWAP)
        mv.visitVarInsn(ALOAD, variable + ctxParameterOffset)
        //now ctx, newvalue, oldvalue on stack
        writeVCreateChoice(mv)
        //now new choice value on stack combining old and new value
        mv.visitVarInsn(ASTORE, variable + ctxParameterOffset)
    }

    override def getVariables() = Set(variable + ctxParameterOffset)
}

case class InstrILOAD(variable: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit =
        mv.visitVarInsn(ILOAD, variable)

    override def toVByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit = {
        mv.visitVarInsn(ALOAD, variable + ctxParameterOffset)
    }

    override def getVariables() = Set(variable + ctxParameterOffset)
}




