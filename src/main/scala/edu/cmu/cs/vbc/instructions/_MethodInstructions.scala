package edu.cmu.cs.vbc.instructions

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

/**
  * @author chupanw
  */

/**
  * INVOKESPECIAL instruction
  * @param owner
  * @param name
  * @param desc
  * @param itf
  */
case class InstrINVOKESPECIAL(owner: String, name: String, desc: String, itf: Boolean) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKESPECIAL, owner, name, desc, itf)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    // TODO: not complete, desc may need to be modified
    // TODO: if the method is invoked on a conditional object, we need VOP to do the invoke
    mv.visitMethodInsn(INVOKESPECIAL, owner, name, desc, itf)
  }
}


/**
  * INVOKEVIRTUAL instruction TODO: seems duplicate, compress all the method instruction?
  * @param owner
  * @param name
  * @param desc
  * @param itf
  */
case class InstrINVOKEVIRTUAL(owner: String, name: String, desc: String, itf: Boolean) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, desc, itf)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    //TODO: require VOP to do the invoke if method is invoked on conditional object
    if (owner.startsWith("java/")) {
      // library code
      mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, desc, itf)
    }
    else {
      loadFExpr(mv, env, env.getBlockVar(block))
      mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, liftMethodDescription(desc), itf)
    }
  }
}


/**
  * INVOKESTATIC
  * @param owner
  * @param name
  * @param desc
  * @param itf
  */
case class InstrINVOKESTATIC(owner: String, name: String, desc: String, itf: Boolean) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKESTATIC, owner, name, desc, itf)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKESTATIC, owner, name, desc, itf)
  }
}