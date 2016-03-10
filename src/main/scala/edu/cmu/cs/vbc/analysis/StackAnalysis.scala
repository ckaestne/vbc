package edu.cmu.cs.vbc.analysis

import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis.{Analyzer, BasicInterpreter, BasicValue, Frame}

/**
  * Perform basic stack analysis
  *
  */
class StackAnalysis(owner: String, mn: MethodNode) extends Analyzer[BasicValue](new BasicInterpreter()) {

  private val framesBefore = analyzeBefore()
  private val framesAfter = analyzeAfter()

  def getFramesBefore: Array[Frame[BasicValue]] = {
    (for (
      i <- framesBefore.indices
      if !mn.instructions.get(i).isInstanceOf[FrameNode]
      if !mn.instructions.get(i).isInstanceOf[LabelNode]
      if !mn.instructions.get(i).isInstanceOf[LineNumberNode]
    ) yield framesBefore(i)).toArray
  }

  def getFramesAfter: Array[Frame[BasicValue]] = {
    (for (
      i <- framesAfter.indices
      if !mn.instructions.get(i).isInstanceOf[FrameNode]
      if !mn.instructions.get(i).isInstanceOf[LabelNode]
      if !mn.instructions.get(i).isInstanceOf[LineNumberNode]
    ) yield framesAfter(i)).toArray
  }

  private def analyzeBefore(): Array[Frame[BasicValue]] = {
    analyze(owner, mn)
  }

  def analyzeAfter(): Array[Frame[BasicValue]] = {
    val basicInterpreter = new BasicInterpreter()
    (for (i <- framesBefore.indices if framesBefore(i) != null) yield {
      val insn = mn.instructions.get(i)
      val copied = new Frame[BasicValue](framesBefore(i))
      val insnType = insn.getType
      if (insnType == AbstractInsnNode.LABEL || insnType == AbstractInsnNode.LINE || insnType == AbstractInsnNode.FRAME)
        copied
      else {
        copied.execute(insn, basicInterpreter)
        copied
      }
    }).toArray
  }
}