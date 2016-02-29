package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

/**
  * @author chupanw
  */

/**
  * INVOKESPECIAL instruction
  *
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
        // TODO: better filering
        if (owner.startsWith("java/")) {
            mv.visitMethodInsn(INVOKESPECIAL, owner, name, desc, itf)
        }
        else {
            loadFExpr(mv, env, env.getBlockVar(block))
            mv.visitMethodInsn(INVOKESPECIAL, owner, name, liftMethodDescription(desc), itf)
        }
    }
}


/**
  * INVOKEVIRTUAL instruction TODO: seems duplicate, compress all the method instruction?
  *
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
        //TODO: better filtering
        if (owner.startsWith("java/")) {
            // library code
            // TODO: implement a model class for StringBuilder
            if (owner == "java/lang/StringBuilder" && name == "append") {
                mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, genSign(primitiveToObjectType("O"), "Ljava/lang/StringBuilder;"), itf)
            }
            else if (owner == "java/io/PrintStream" && name == "println") {
                mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, genSign(primitiveToObjectType("O"), "V"), itf)
            }
            else {
                mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, desc, itf)
            }
        }
        else {
            if (env.isMain) pushConstantTRUE(mv) else loadFExpr(mv, env, env.getBlockVar(block))
            mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, liftMethodDescription(desc), itf)
        }
    }
}


/**
  * INVOKESTATIC
  *
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
        //TODO: better filtering
        if (owner.startsWith("java/")) {
            mv.visitMethodInsn(INVOKESTATIC, owner, name, desc, itf)
        }
        else {
            loadFExpr(mv, env, env.getBlockVar(block))
            mv.visitMethodInsn(INVOKESTATIC, owner, name, liftMethodDescription(desc), itf)
        }
    }
}