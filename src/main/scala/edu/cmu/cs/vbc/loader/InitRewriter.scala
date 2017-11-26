package edu.cmu.cs.vbc.loader

import java.util

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis.{Analyzer, SourceInterpreter, SourceValue}

import scala.collection.JavaConversions._
import scala.collection.mutable

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
    * ALOAD 0
    * {load arguments}
    * INVOKESPECIAL superclass's init
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
    if (c.name == "java/lang/Object") return m;

    val initAnalyzer = new InitAnalyzer(c, m)
    val analyzer = new Analyzer[SourceValue](initAnalyzer)
    analyzer.analyze(c.name, m)

    val instructions = m.instructions.toArray
    m.instructions.clear()

    assert(initAnalyzer.invokeSpecialOfSuperClss.size <= 1, "calling more than one <init> of superclass")
    assert(initAnalyzer.invokeSpecialOfSuperClss.size + initAnalyzer.invokeSpecialOfSameClass.size > 0, "no supercall found in constructor")

    val (aload0Idx, invokeSpecialIdx): (Int, Int) =
      if (initAnalyzer.invokeSpecialOfSuperClss.size == 1) {
        initAnalyzer.invokeSpecialOfSuperClss.head
      } else {
        assert(initAnalyzer.invokeSpecialOfSameClass.size == 1, "calling more than one <init> of the same class")
        initAnalyzer.invokeSpecialOfSameClass.head
      }
    val (prefix, rest) = instructions.splitAt(aload0Idx) // prefix not including ALOAD0
    val (initSeq, postfix) = rest.splitAt(invokeSpecialIdx + 1 - aload0Idx) // postfix not including invokespecial
    (initSeq ++ prefix ++ postfix) foreach { i => m.instructions.add(i) }
    m
  }
}

class InitAnalyzer(cn: ClassNode, mn: MethodNode) extends SourceInterpreter {
  /**
    * Pairs of ALOAD 0 index and INVOKESPECIAL index
    */
  val invokeSpecialOfSuperClss: mutable.Set[(Int, Int)] = mutable.Set()
  val invokeSpecialOfSameClass: mutable.Set[(Int, Int)] = mutable.Set()

  override def naryOperation(insn: AbstractInsnNode, values: util.List[_ <: SourceValue]) = insn match {
    case methodInsn: MethodInsnNode =>
      val methodInsnIndex = mn.instructions.indexOf(insn)
      if (methodInsn.getOpcode == Opcodes.INVOKESPECIAL && methodInsn.name == "<init>") {
        val ref = values.get(0)
        val refSources = ref.insns.toSet.filter(i => i.isInstanceOf[VarInsnNode] && i.getOpcode == Opcodes.ALOAD && i.asInstanceOf[VarInsnNode].`var` == 0)
        val sourceIndexes = refSources.map(mn.instructions.indexOf(_))
        if (methodInsn.owner == cn.superName)
          sourceIndexes.foreach(aloadIdx => invokeSpecialOfSuperClss.add((aloadIdx, methodInsnIndex)))
        else if (methodInsn.owner == cn.name)
          sourceIndexes.foreach(aloadIdx => invokeSpecialOfSameClass.add((aloadIdx, methodInsnIndex)))
      }
      super.naryOperation(insn, values)
    case _ => super.naryOperation(insn, values)
  }
}
