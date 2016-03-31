package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.{INT_TYPE, VBCFrame, VBCType, V_TYPE}
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{MethodVisitor, Type}

/**
  * @author chupanw
  */

/**
  * ICONST_n instruction
  *
  * @param v
  */
case class InstrICONST(v: Int) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    pushConstant(mv, v)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      pushConstant(mv, v)
      mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
      callVCreateOne(mv)
    }
    else {
      pushConstant(mv, v)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Option[Instruction]) = {
    val newFrame =
      if (env.shouldLiftInstr(this))
        s.push(V_TYPE(), Some(this))
      else
        s.push(INT_TYPE(), Some(this))
    (newFrame, None)
  }
}


case class InstrLDC(o: Object) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitLdcInsn(o)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, blockA: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      mv.visitLdcInsn(o)
      //TODO: wrap into a V
      if (!o.isInstanceOf[String]) {
        if (o.isInstanceOf[Integer]) {
          mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
        }
      }
      callVCreateOne(mv)
    }
    else {
      mv.visitLdcInsn(o)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Option[Instruction]) = {
    val newFrame =
      if (env.shouldLiftInstr(this))
        s.push(V_TYPE(), Some(this))
      else
        o match {
          case i: java.lang.Integer => s.push(INT_TYPE(), Some(this))
          case str: java.lang.String => s.push(VBCType(Type.getObjectType("java/lang/String")), Some(this))
          case _ => throw new RuntimeException("Incomplete support for LDC")
        }
    (newFrame, None)
  }
}

case class InstrACONST_NULL() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = mv.visitInsn(ACONST_NULL)

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      mv.visitInsn(ACONST_NULL)
      callVCreateOne(mv)
    }
    else {
      mv.visitInsn(ACONST_NULL)
    }

  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Option[Instruction]) = {
    if (env.shouldLiftInstr(this))
      (s.push(V_TYPE(), Some(this)), None)
    else
      (s.push(VBCType(Type.getObjectType("null")), Some(this)), None)
  }
}