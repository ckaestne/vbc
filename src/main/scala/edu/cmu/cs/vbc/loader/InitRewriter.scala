package edu.cmu.cs.vbc.loader

import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._

/**
  * Rewrite init methods so that init sequence is ahead of anything else.
  *
  * This rewriting happens before the variational lifting.
  *
  * @author chupanw
  */
object InitRewriter {

  /**
    * We assume the following sequence, and they could span across multiple blocks
    * in the bytecode.
    *   ALOAD 0
    *   {load arguments}
    *   INVOKESPECIAL superclass's init
    *
    * We need to extract Object creation sequence and put them to the beginning.
    * We cannot keep the original instruction order because our lifting of PUTFIELD would call
    * GETFIELD, which would cause problems if current object is not initialized.
    * [[edu.cmu.cs.vbc.prog.LinkedListExample]] is an example of this, where in the original
    * bytecode, there is a PUTFIELD before Object.init() (PUTFIELD before object initialization
    * is allowed in JVM).
    *
    * To do that, we find the last INVOKESPECIAL call of init method, and then search reversely for
    * corresponding ALOAD0 instructions. This sequence should be the init sequence we are looking for.
    *
    * @param m Method being rewrite, could be methods other than init, which will be returned
    *          directly.
    * @return Rewritten method. To avoid missing some fields while creating new MethodNode, we modify
    *         the instruction list in place.
    */
  def extractInitSeq(m: MethodNode, c: ClassNode): MethodNode = {
    if (m.name != "<init>") return m
    val instructions = m.instructions.toArray
    m.instructions.clear()
    val nInit = instructions.count(isINVOKESPECIALInit(_, c))
    val lastInitIdx = instructions.lastIndexWhere(isINVOKESPECIALInit(_, c))
    val aloadIdx = instructions.take(lastInitIdx).zipWithIndex.filter(p => isALOAD0(p._1)).reverse.take(nInit).last._2
    val (prefix, rest) = instructions.splitAt(aloadIdx) // prefix not including ALOAD0
    val (initSeq, postfix) = rest.splitAt(lastInitIdx + 1 - aloadIdx) // postfix not including invokespecial
    (initSeq ++ prefix ++ postfix) foreach {i => m.instructions.add(i)}
    m
  }

  private def isINVOKESPECIALInit(i: AbstractInsnNode, c: ClassNode): Boolean = i match {
    case method: MethodInsnNode => method.getOpcode == INVOKESPECIAL &&
      method.name.contentEquals("<init>") &&
      (method.owner.contentEquals(c.superName) || method.owner.contentEquals(c.name))
    case _ => false
  }

  private def isALOAD0(i: AbstractInsnNode): Boolean = i match {
    case varInsn: VarInsnNode => varInsn.getOpcode == ALOAD && varInsn.`var` == 0
    case _ => false
  }
}
