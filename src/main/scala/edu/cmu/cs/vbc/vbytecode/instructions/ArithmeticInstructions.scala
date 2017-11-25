package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.VBCFrame.UpdatedFrame
import edu.cmu.cs.vbc.analysis._
import edu.cmu.cs.vbc.utils.LiftUtils._
import edu.cmu.cs.vbc.utils.{InvokeDynamicUtils, VCall}
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._


abstract class BinOpInstruction(op: Int, vopsMethod: String, retType: VBCType) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(op)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(
        INVOKESTATIC,
        Owner.getVOps,
        vopsMethod,
        s"($vclasstype$vclasstype$fexprclasstype)$vclasstype",
        false
      )
    }
    else
      mv.visitInsn(op)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = {
    val retVType: VBCType = retType match {
      case _: LONG_TYPE => V_TYPE(true)
      case _: DOUBLE_TYPE => V_TYPE(true)
      case _ => V_TYPE(false)
    }
    if (s.stack.take(2).exists(_._1.isInstanceOf[V_TYPE]))
      env.setLift(this)
    val (v1, prev1, frame1) = s.pop()
    val (v2, prev2, frame2) = frame1.pop()
    val newFrame =
      if (env.shouldLiftInstr(this))
        frame2.push(retVType, Set(this))
      else {
        frame2.push(retType, Set(this))
      }
    val backtrack: Set[Instruction] =
      if (env.shouldLiftInstr(this)) {
        if (!v1.isInstanceOf[V_TYPE]) prev1
        else if (!v2.isInstanceOf[V_TYPE]) prev2
        else Set()
      }
      else
        Set()
    (newFrame, backtrack)
  }

}

abstract class BinIntOpInstruction(op: Int, vopsMethod: String) extends BinOpInstruction(op, vopsMethod, INT_TYPE())

case class InstrIADD() extends BinIntOpInstruction(IADD, "IADD")

case class InstrISUB() extends BinIntOpInstruction(ISUB, "ISUB")

case class InstrIMUL() extends BinIntOpInstruction(IMUL, "IMUL")

case class InstrIDIV() extends BinIntOpInstruction(IDIV, "IDIV")

/**
  * Negate int.
  *
  * Operand stack: ..., value -> ..., result
  */
case class InstrINEG() extends UnaryOpInstruction(INEG, "ineg", INT_TYPE())
case class InstrFNEG() extends UnaryOpInstruction(FNEG, "fneg", FLOAT_TYPE())
case class InstrDNEG() extends UnaryOpInstruction(DNEG, "dneg", DOUBLE_TYPE())

abstract class UnaryOpInstruction(op: Int, vopsMethod: String, retType: VBCType) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(op)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = {
    val (v, prev, frame) = s.pop()
    if (v == V_TYPE(false))
      env.setLift(this)
    val newFrame =
      if (env.shouldLiftInstr(this))
        frame.push(V_TYPE(false), Set(this))
      else {
        frame.push(retType, Set(this))
      }
    if (env.shouldLiftInstr(this) && v != V_TYPE(is64Bit = false))
      return (s, prev)
    (newFrame, Set())
  }

  /**
    * Lifting means performing operations on a V object
    */
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(
        INVOKESTATIC,
        Owner.getVOps,
        vopsMethod,
        s"($vclasstype$fexprclasstype)$vclasstype",
        false
      )
    }
    else {
      mv.visitInsn(op)
    }
  }
}

/** Shift left int
  *
  * ..., value1(int), value2(int) -> ..., result
  */
case class InstrISHL() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(ISHL)
  }

  /** Lifting means invoking ISHL on V.
    *
    * If lifting, assume that value2 is V
    */
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      InvokeDynamicUtils.invoke(
        VCall.sflatMap,
        mv,
        env,
        loadCtx = loadCurrentCtx(_, env, block),
        lambdaName = "ISHL",
        desc = TypeDesc.getInt + "(" + TypeDesc.getInt + ")" + vclasstype,
        nExplodeArgs = 1
      ) {
        (mv: MethodVisitor) => {
          mv.visitVarInsn(ALOAD, 0) // Integer
          Integer2int(mv) // int
          mv.visitVarInsn(ALOAD, 2) // Integer
          Integer2int(mv) // int
          mv.visitInsn(ISHL)
          int2Integer(mv)
          callVCreateOne(mv, m => m.visitVarInsn(ALOAD, 1))
          mv.visitInsn(ARETURN)
        }
      }
    }
    else
      mv.visitInsn(ISHL)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = {
    val (argType, argBacktrack, frame1) = s.pop()
    val (receiverType, receiverBacktrack, frame2) = frame1.pop()
    val hasV = argType == V_TYPE(false) || receiverType == V_TYPE(false)
    if (hasV) env.setLift(this)
    val newFrame = frame2.push(if (hasV) V_TYPE(false) else INT_TYPE(), Set(this))
    val backtrack: Set[Instruction] = (argType, receiverType) match {
      case (INT_TYPE(), _) => argBacktrack
      case (_, INT_TYPE()) => receiverBacktrack
      case _ => Set()
    }
    (newFrame, backtrack)
  }
}

/** Compare long
  *
  * ..., value1(long), value2(long) -> ..., result(int)
  */
case class InstrLCMP() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(LCMP)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      InvokeDynamicUtils.invoke(
        VCall.sflatMap,
        mv,
        env,
        loadCtx = loadCurrentCtx(_, env, block),
        lambdaName = "lcmp",
        desc = TypeDesc.getLong + s"(${TypeDesc.getLong})" + vclasstype,
        nExplodeArgs = 1
      ) {
        (mv: MethodVisitor) => {
          mv.visitVarInsn(ALOAD, 0) //value1
          Long2long(mv)
          mv.visitVarInsn(ALOAD, 2) //value2
          Long2long(mv)
          mv.visitInsn(LCMP)
          int2Integer(mv)
          callVCreateOne(mv, (m) => m.visitVarInsn(ALOAD, 1))
          mv.visitInsn(ARETURN)
        }
      }
    }
    else
      mv.visitInsn(LCMP)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = {
    val (value2Type, value2Backtrack, frame2) = s.pop()
    val (value1Type, value1Backtrack, frame1) = frame2.pop()
    ((value1Type, value2Type): @unchecked) match {
      case (LONG_TYPE(), LONG_TYPE()) => (frame1.push(INT_TYPE(), Set(this)), Set())
      case (LONG_TYPE(), V_TYPE(true)) => (frame1, value1Backtrack) // backtrack, frame1 will be discarded
      case (V_TYPE(true), LONG_TYPE()) => (frame1, value2Backtrack) // backtrack, frame2 will be discarded
      case (V_TYPE(true), V_TYPE(true)) =>
        env.setLift(this)
        (frame1.push(V_TYPE(false), Set(this)), Set())
    }
  }
}

/** Negate long
  *
  * ..., value(long) -> ..., result(long)
  */
case class InstrLNEG() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(LNEG)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      InvokeDynamicUtils.invoke(
        VCall.smap,
        mv,
        env,
        loadCtx = loadCurrentCtx(_, env, block),
        lambdaName = "lneg",
        desc = TypeDesc.getLong + "()" + TypeDesc.getLong
      ) {
        (mv: MethodVisitor) => {
          mv.visitVarInsn(ALOAD, 1)
          Long2long(mv)
          mv.visitInsn(LNEG)
          long2Long(mv)
          mv.visitInsn(ARETURN)
        }
      }
    }
    else
      mv.visitInsn(LNEG)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = {
    val (vType, _, frame) = s.pop()
    if (vType == V_TYPE(true)) {
      env.setLift(this)
      (frame.push(V_TYPE(true), Set(this)), Set())
    }
    else
      (frame.push(LONG_TYPE(), Set(this)), Set())
  }
}

/** Arithmetic shift right int
  *
  * ..., value1(int), value2(int) -> ..., result(int)
  */
case class InstrISHR() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(ISHR)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      InvokeDynamicUtils.invoke(
        VCall.sflatMap,
        mv,
        env,
        loadCtx = loadCurrentCtx(_, env, block),
        lambdaName = "ishr",
        desc = TypeDesc.getInt + s"(${TypeDesc.getInt})" + vclasstype,
        nExplodeArgs = 1
      ) {
        (mv: MethodVisitor) => {
          mv.visitVarInsn(ALOAD, 0) // value1
          Integer2int(mv)
          mv.visitVarInsn(ALOAD, 2) // value2
          Integer2int(mv)
          mv.visitInsn(ISHR)
          int2Integer(mv)
          callVCreateOne(mv, (m) => m.visitVarInsn(ALOAD, 1))
          mv.visitInsn(ARETURN)
        }
      }
    }
    else
      mv.visitInsn(ISHR)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = {
    val (value2Type, value2Backtrack, frame2) = s.pop()
    val (value1Type, value1Backtrack, frame1) = frame2.pop()
    ((value1Type, value2Type): @unchecked) match {
      case (INT_TYPE(), INT_TYPE()) => (frame1.push(INT_TYPE(), Set(this)), Set())
      case (INT_TYPE(), V_TYPE(false)) => (frame1, value1Backtrack)
      case (V_TYPE(false), INT_TYPE()) => (frame1, value2Backtrack)
      case (V_TYPE(false), V_TYPE(false)) =>
        env.setLift(this)
        (frame1.push(V_TYPE(false), Set(this)), Set())
    }
  }
}

/**
  * Boolean AND int
  */
case class InstrIAND() extends BinIntOpInstruction(IAND, "iand")

case class InstrLAND() extends BinOpInstruction(LAND, "land", LONG_TYPE())

/** Boolean OR int
  *
  * ..., value1(int), value2(int) -> result(int)
  */
case class InstrIOR() extends BinOpInstruction(IOR, "ior", INT_TYPE())

case class InstrLSUB() extends BinOpInstruction(LSUB, "lsub", LONG_TYPE())
case class InstrFSUB() extends BinOpInstruction(FSUB, "fsub", FLOAT_TYPE())
case class InstrDSUB() extends BinOpInstruction(DSUB, "dsub", DOUBLE_TYPE())

/** Logical shift right int
  *
  * ..., value1(int), value2(int) -> ..., result(int)
  */
case class InstrIUSHR() extends BinOpInstruction(IUSHR, "iushr", INT_TYPE())

/** Remainder int
  *
  * ..., value1(int), value2(int) -> ..., result(int)
  */
case class InstrIREM() extends BinOpInstruction(IREM, "irem", INT_TYPE())
case class InstrDREM() extends BinOpInstruction(DREM, "drem", DOUBLE_TYPE())
case class InstrFREM() extends BinOpInstruction(FREM, "frem", FLOAT_TYPE())

/** Boolean XOR int
  *
  * ..., value1, value2 -> ..., result
  */
case class InstrIXOR() extends BinOpInstruction(IXOR, "ixor", INT_TYPE())


/** Convert long to int
  *
  * ..., value(long) -> ..., result(int)
  */
case class InstrL2I() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
    mv.visitInsn(L2I)

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(
        INVOKESTATIC,
        Owner.getVOps,
        "l2i",
        s"($vclasstype$fexprclasstype)$vclasstype",
        false
      )
    }
    else
      mv.visitInsn(L2I)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = {
    val (v, prev, frame) = s.pop()
    if (v == V_TYPE(true))
      env.setLift(this)
    val newFrame =
      if (env.shouldLiftInstr(this))
        frame.push(V_TYPE(false), Set(this))
      else {
        frame.push(INT_TYPE(), Set(this))
      }
    if (env.shouldLiftInstr(this) && v != V_TYPE(true))
      return (s, prev)
    (newFrame, Set())
  }
}

/** Convert int to short
  *
  * ..., value (int) -> ..., result (int)
  */
case class InstrI2S() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
    mv.visitInsn(I2S)

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(
        INVOKESTATIC,
        Owner.getVOps,
        "i2s",
        s"($vclasstype$fexprclasstype)$vclasstype",
        false
      )
    }
    else
      mv.visitInsn(I2S)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = {
    val (v, prev, frame) = s.pop()
    if (v == V_TYPE(false))
      env.setLift(this)
    val newFrame =
      if (env.shouldLiftInstr(this))
        frame.push(V_TYPE(false), Set(this))
      else {
        frame.push(INT_TYPE(), Set(this))
      }
    if (env.shouldLiftInstr(this) && v != V_TYPE(false))
      return (s, prev)
    (newFrame, Set())

  }
}

/** Logical shift right long
  *
  * ..., value1(long), value2(int) -> ..., result(long)
  */
case class InstrLUSHR() extends BinOpInstruction(LUSHR, "lushr", LONG_TYPE())

/** Divide long
  *
  * ..., value1(long), value2(long) -> ..., result(long)
  */
case class InstrLDIV() extends BinOpInstruction(LDIV, "ldiv", LONG_TYPE())


/** Add long
  *
  * ..., value1(long), value2(long) -> ..., result(long)
  * todo: should extend BinOps
  */
case class InstrLADD() extends BinOpInstruction(LADD, "ladd", LONG_TYPE())


case class InstrDMUL() extends BinOpInstruction(DMUL, "dmul", DOUBLE_TYPE())

/**
  * Compare double
  *
  * ..., value1(double), value2(double) -> ..., result(int)
  *
  * If value1 is greater than value2, 1(int) is pushed onto the operand stack.
  * If value1 is equal to value2, 0 is pushed.
  * If value1 is less than value2, -1 is pushed.
  * If at least one of value1 and value2 is NaN, -1 is pushed.
  */
case class InstrDCMPL() extends BinOpInstruction(DCMPL, "dcmpl", INT_TYPE())


case class InstrLOR() extends BinOpInstruction(LOR, "lor", LONG_TYPE())

/**
  * ..., value1(long), value2(int) -> ..., result(long)
  */
case class InstrLSHL() extends BinOpInstruction(LSHL, "lshl", LONG_TYPE())

trait __replace_BinOpNonIntInstruction extends Instruction {
  def updateStackWithReturnType(s: VBCFrame, env: VMethodEnv, retType: VBCType): (VBCFrame, Set[Instruction]) = {
    val retVType: VBCType = retType match {
      case _: LONG_TYPE => V_TYPE(true)
      case _: DOUBLE_TYPE => V_TYPE(true)
      case _ => V_TYPE(false)
    }
    if (s.stack.take(2).exists(_._1.isInstanceOf[V_TYPE]))
      env.setLift(this)
    val (v1, prev1, frame1) = s.pop()
    val (v2, prev2, frame2) = frame1.pop()
    val newFrame =
      if (env.shouldLiftInstr(this))
        frame2.push(retVType, Set(this))
      else {
        frame2.push(retType, Set(this))
      }
    val backtrack: Set[Instruction] =
      if (env.shouldLiftInstr(this)) {
        if (!v1.isInstanceOf[V_TYPE]) prev1
        else if (!v2.isInstanceOf[V_TYPE]) prev2
        else Set()
      }
      else
        Set()
    (newFrame, backtrack)
  }
}

/**
  * value1 (long), value2 (int) -> result (long)
  */
case class InstrLSHR() extends __replace_BinOpNonIntInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = mv.visitInsn(LSHR)

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, "lshr", s"($vclasstype$vclasstype$fexprclasstype)$vclasstype", false)
    }
    else
      mv.visitInsn(LSHR)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = {
    val (value2Type, value2Backtrack, frame2) = s.pop()
    val (value1Type, value1Backtrack, frame1) = frame2.pop()
    ((value1Type, value2Type): @unchecked) match {
      case (LONG_TYPE(), INT_TYPE()) => (frame1.push(LONG_TYPE(), Set(this)), Set())
      case (LONG_TYPE(), V_TYPE(false)) => (frame1, value1Backtrack)
      case (V_TYPE(true), INT_TYPE()) => (frame1, value2Backtrack)
      case (V_TYPE(true), V_TYPE(false)) =>
        env.setLift(this)
        (frame1.push(V_TYPE(true), Set(this)), Set())
    }
  }
}

case class InstrLXOR() extends __replace_BinOpNonIntInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(LXOR)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, "lxor", s"($vclasstype$vclasstype$fexprclasstype)$vclasstype", false)
    }
    else
      mv.visitInsn(LXOR)

  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = updateStackWithReturnType(s, env, LONG_TYPE())
}

case class InstrLMUL() extends __replace_BinOpNonIntInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(LMUL)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, "lmul", s"($vclasstype$vclasstype$fexprclasstype)$vclasstype", false)
    }
    else
      mv.visitInsn(LMUL)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = updateStackWithReturnType(s, env, LONG_TYPE())
}

/**
  * ..., value1 (double), value2 (double) -> ..., result (int)
  */
case class InstrDCMPG() extends __replace_BinOpNonIntInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(DCMPG)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, "dcmpg", s"($vclasstype$vclasstype$fexprclasstype)$vclasstype", false)
    } else {
      mv.visitInsn(DCMPG)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = {
    if (s.stack.take(2).exists(_._1 == V_TYPE(true)))
      env.setLift(this)
    val (v1, prev1, frame1) = s.pop()
    val (v2, prev2, frame2) = frame1.pop()
    val newFrame =
      if (env.shouldLiftInstr(this))
        frame2.push(V_TYPE(false), Set(this))
      else {
        frame2.push(INT_TYPE(), Set(this))
      }
    val backtrack: Set[Instruction] =
      if (env.shouldLiftInstr(this)) {
        if (v1 != V_TYPE(true)) prev1
        else if (v2 != V_TYPE(true)) prev2
        else Set()
      }
      else
        Set()
    (newFrame, backtrack)
  }
}

/**
  * ..., value1 (double), value2 (double) -> ..., result (double)
  */
case class InstrDDIV() extends __replace_BinOpNonIntInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(DDIV)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, "ddiv", s"($vclasstype$vclasstype$fexprclasstype)$vclasstype", false)
    } else {
      mv.visitInsn(DDIV)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = updateStackWithReturnType(s, env, DOUBLE_TYPE())
}

case class InstrDADD() extends __replace_BinOpNonIntInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = mv.visitInsn(DADD)

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, "dadd", s"($vclasstype$vclasstype$fexprclasstype)$vclasstype", false)
    } else {
      mv.visitInsn(DADD)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = updateStackWithReturnType(s, env, DOUBLE_TYPE())
}

case class InstrLREM() extends __replace_BinOpNonIntInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = mv.visitInsn(LREM)

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, "lrem", s"($vclasstype$vclasstype$fexprclasstype)$vclasstype", false)
    } else {
      mv.visitInsn(LREM)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = updateStackWithReturnType(s, env, LONG_TYPE())
}

/**
  * Divide float
  *
  * ..., value1 (float), value2 (float) -> ..., result (float)
  */
case class InstrFDIV() extends __replace_BinOpNonIntInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = mv.visitInsn(FDIV)

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, "fdiv", s"($vclasstype$vclasstype$fexprclasstype)$vclasstype", false)
    } else {
      mv.visitInsn(FDIV)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = updateStackWithReturnType(s, env, FLOAT_TYPE())
}

/**
  * Compare float
  *
  * ..., value1 (float), value2 (float) -> ..., result (int)
  */
case class InstrFCMPG() extends BinOpInstruction(FCMPG, "fcmpg", INT_TYPE())

case class InstrFCMPL() extends BinOpInstruction(FCMPL, "fcmpl", INT_TYPE())

case class InstrFMUL() extends __replace_BinOpNonIntInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = mv.visitInsn(FMUL)

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, "fmul", s"($vclasstype$vclasstype$fexprclasstype)$vclasstype", false)
    } else {
      mv.visitInsn(FMUL)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = updateStackWithReturnType(s, env, FLOAT_TYPE())
}

case class InstrFADD() extends __replace_BinOpNonIntInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = mv.visitInsn(FADD)

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, "fadd", s"($vclasstype$vclasstype$fexprclasstype)$vclasstype", false)
    } else {
      mv.visitInsn(FADD)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = updateStackWithReturnType(s, env, FLOAT_TYPE())
}