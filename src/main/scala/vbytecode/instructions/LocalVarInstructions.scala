package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.{INT_TYPE, REF_TYPE, VBCFrame}
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

/**
  * ISTORE instruction
  *
  * @param variable
  */
case class InstrISTORE(variable: Variable) extends Instruction {
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

    override def getVariables() = {
        variable match {
            case p: Parameter => Set()
            case lv: LocalVar => Set(lv)
        }
    }

    override def updateStack(s: VBCFrame) = s.store(variable, INT_TYPE(), this)
}


/**
  * ILOAD instruction
  *
  * @param variable
  */
case class InstrILOAD(variable: Variable) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
        mv.visitVarInsn(ILOAD, env.getVarIdx(variable))

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        loadV(mv, env, variable)
    }

    override def getVariables() = {
        variable match {
            case p: Parameter => Set()
            case lv: LocalVar => Set(lv)
        }
    }

    override def updateStack(s: VBCFrame) = s.load(variable, INT_TYPE(), this)
}


/**
  * IINC instruction
  *
  * @param variable
  * @param increment
  */
case class InstrIINC(variable: Variable, increment: Int) extends Instruction {
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

    override def getVariables() = {
        variable match {
            case p: Parameter => Set()
            case lv: LocalVar => Set(lv)
        }
    }

    override def updateStack(s: VBCFrame) = {
        //does not change the frame
        assert(s.localVar contains variable, "local variable not assigned")
        assert(s.localVar(variable)._1 == INT_TYPE(), "local variable not of type Int")
        s
    }

}


/**
  * ALOAD instruction
  */
case class InstrALOAD(variable: Variable) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        val idx = env.getVarIdx(variable)
        mv.visitVarInsn(ALOAD, idx)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        val idx = env.getVarIdx(variable)
        mv.visitVarInsn(ALOAD, idx)
    }

    override def getVariables() = {
        variable match {
            case p: Parameter => Set()
            case lv: LocalVar => Set(lv)
        }
    }

    /**
      * Used to identify the start of init method
      *
      * @see [[Rewrite.rewrite()]]
      */
    override def isALOAD0: Boolean = variable.getIdx().contains(0)
    override def updateStack(s: VBCFrame) = s.load(variable, REF_TYPE(), this)
}


/**
  * ASTORE: store reference into local variable
  *
  * @param variable
  */
case class InstrASTORE(variable: Variable) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        val idx = env.getVarIdx(variable)
        mv.visitVarInsn(ASTORE, idx)
    }

    //TODO: make this variational
    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        val idx = env.getVarIdx(variable)
        mv.visitVarInsn(ASTORE, idx)
    }

    override def getVariables = {
        variable match {
            case p: Parameter => Set()
            case lv: LocalVar => Set(lv)
        }
    }

    override def updateStack(s: VBCFrame) = s.store(variable, REF_TYPE(), this)
}

