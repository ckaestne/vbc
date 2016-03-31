package edu.cmu.cs.vbc.analysis

import edu.cmu.cs.vbc.analysis.VBCFrame.FrameEntry
import edu.cmu.cs.vbc.vbytecode.Variable
import edu.cmu.cs.vbc.vbytecode.instructions._

object VBCFrame {
  // an entry consists of the type of the entry and the previous instruction that is responsible for
  // setting this entry
  type FrameEntry = (VBCType, Option[Instruction])

  type UpdatedFrame = (VBCFrame, Option[Instruction])
}

/**
  * For each instruction, Frame contains information about local variables and stack elements.
  *
  * Local variables and stacks both store the type of the entry and the instruction that last updated
  * the entry (in case one wants to later lift the operation that creates this entry)
  *
  */
case class VBCFrame(localVar: Map[Variable, FrameEntry], stack: List[FrameEntry]) {

  /**
    * Set the value for local variables
    */
  def setLocal(v: Variable, vtype: VBCType, instr: Option[Instruction]): VBCFrame =
    this.copy(localVar = localVar + (v ->(vtype, instr)))

  /**
    * Merge two frames, for each variables, set the value to TOP if differs
    */
  def merge(that: VBCFrame): VBCFrame = {
    if (this == that)
      this
    else if (this.stack.size != that.stack.size) {
      throw new RuntimeException("Incompatible stack heights")
    }
    else {
      VBCFrame(
        // merge locla variables (when defined only in one branch, it cannot be initialized)
        mapMerge(this.localVar, that.localVar, (a: Option[FrameEntry], b: Option[FrameEntry]) => {
          (a, b) match {
            case (None, Some(v)) => (v._1, v._2)
            case (Some(v), None) => (v._1, v._2)
            case (Some(v1), Some(v2)) => mergeFrameEntry(v1, v2)
            case (None, None) => throw new RuntimeException("should not happen")
          }
        }),
        (this.stack zip that.stack).map(v => mergeFrameEntry(v._1, v._2))
      )
    }
  }

  private def mergeFrameEntry(v1: FrameEntry, v2: FrameEntry): FrameEntry = (v2._1, v2._2)

  private def mapMerge[K, V](m1: Map[K, V], m2: Map[K, V], fun: (Option[V], Option[V]) => V): Map[K, V] =
    (m1.keySet ++ m2.keySet) map { i => i -> fun(m1.get(i), m2.get(i)) } toMap

  /**
    * push to the stack
    */
  def push(v: VBCType, instr: Option[Instruction]): VBCFrame = {
    this.copy(stack = (v, instr) :: stack)
  }

  /**
    * pop one value from stack
    */
  def pop(): (VBCType, Option[Instruction], VBCFrame) = {
    if (stack.isEmpty)
      throw new IndexOutOfBoundsException("Cannot pop operand off an empty stack.")
    val entry = stack.head
    (entry._1, entry._2, this.copy(stack = stack.tail))
  }

  override def toString: String = {
    val sb = new StringBuilder
    //    if (isInCtx) sb.append("[x]") else sb.append("[ ]")
    sb.append("stack: ")
    stack.foreach((tuple) => sb.append(tuple._1))
    sb.toString()
  }

  def getStackSize = stack.length
}
