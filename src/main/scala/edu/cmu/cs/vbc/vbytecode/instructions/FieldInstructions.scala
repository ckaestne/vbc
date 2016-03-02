package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

/**
  * @author chupanw
  */

/**
  * GETSTATIC
  *
  * @param owner
  * @param name
  * @param desc
  */
case class InstrGETSTATIC(owner: String, name: String, desc: String) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        mv.visitFieldInsn(GETSTATIC, owner, name, desc)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        //TODO: work for println, but what if we are getting conditional?
        mv.visitFieldInsn(GETSTATIC, owner, name, desc)
    }
}

/**
  * PUTSTATIC
  *
  * @param owner
  * @param name
  * @param desc
  */
case class InstrPUTSTATIC(owner: String, name: String, desc: String) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        mv.visitFieldInsn(PUTSTATIC, owner, name, desc)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        //TODO: work for println, but what if we are getting conditional?
        mv.visitFieldInsn(PUTSTATIC, owner, name, desc)
    }
}

/**
  * GETFIELD
  *
  * @param owner
  * @param name
  * @param desc
  */
case class InstrGETFIELD(owner: String, name: String, desc: String) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        mv.visitFieldInsn(GETFIELD, owner, name, desc)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        mv.visitFieldInsn(GETFIELD, owner, name, "Ledu/cmu/cs/varex/V;")
    }
}


/**
  * PUTFIELD
  *
  * @param owner
  * @param name
  * @param desc
  */
case class InstrPUTFIELD(owner: String, name: String, desc: String) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        mv.visitFieldInsn(PUTFIELD, owner, name, desc)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        // Avoid modifying conditional fields
        if (env.isConditionalField(owner, name, desc)) {
            mv.visitInsn(POP)   // discard the new values for conditional fields
            mv.visitInsn(POP)   // discard object reference
        }
        mv.visitFieldInsn(PUTFIELD, owner, name, "Ledu/cmu/cs/varex/V;")
    }
}