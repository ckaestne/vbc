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

case class InstrICONST(v: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit = {
        writeConstant(mv, v)
    }

    override def toVByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit = {
        writeConstant(mv, v)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
        mv.visitMethodInsn(INVOKESTATIC, vclassname, "one", "(Ljava/lang/Object;)Ledu/cmu/cs/varex/V;", true)
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
        mv.visitVarInsn(ASTORE, variable + ctxParameterOffset)
    }

    override def getVariables() = Set(variable)
}

case class InstrISTORE(variable: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit =
        mv.visitVarInsn(ISTORE, variable)

    override def toVByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit = {
        mv.visitVarInsn(ASTORE, variable + ctxParameterOffset)
    }

    override def getVariables() = Set(variable)
}

case class InstrILOAD(variable: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit =
        mv.visitVarInsn(ILOAD, variable)

    override def toVByteCode(mv: MethodVisitor, method: MethodNode, block: Block): Unit = {
        mv.visitVarInsn(ALOAD, variable + ctxParameterOffset)
    }

    override def getVariables() = Set(variable)
}




