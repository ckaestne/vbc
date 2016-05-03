package edu.cmu.cs.vbc.analysis

import edu.cmu.cs.vbc.analysis.VBCFrame.FrameEntry
import edu.cmu.cs.vbc.vbytecode.Variable
import edu.cmu.cs.vbc.vbytecode.instructions._

object VBCFrame {
  // an entry consists of the type of the entry and the previous instructions that are responsible for
  // setting this entry
  type FrameEntry = (VBCType, Set[Instruction])

  type UpdatedFrame = (VBCFrame, Set[Instruction])
}

/**
  * For each instruction, Frame contains information about local variables and stack elements.
  *
  * Local variables and stacks both store the type of the entry and the instructions that last updated
  * the entry (in case one wants to later lift the operation that creates this entry)
  *
  */
case class VBCFrame(localVar: Map[Variable, FrameEntry], stack: List[FrameEntry]) {

  /**
    * Set the value for local variables
    */
  def setLocal(v: Variable, vtype: VBCType, instr: Set[Instruction]): VBCFrame =
    this.copy(localVar = localVar + (v ->(vtype, instr)))

  /**
    * Merge two frames
    *
    * @param that the new frame
    */
  def merge(that: VBCFrame): VBCFrame = {
    if (this == that)
      this
    else if (this.stack.size != that.stack.size) {
      throw new RuntimeException("Incompatible stack heights: " + this.stack + " vs " + that.stack)
    }
    else {
      VBCFrame(
        // merge locla variables (when defined only in one branch, it cannot be initialized)
        mapMerge(this.localVar, that.localVar, (a: Option[FrameEntry], b: Option[FrameEntry]) => {
          (a, b) match {
            case (None, Some(v)) => (v._1, v._2)
            case (Some(v), None) => (v._1, v._2) // actually, this should not happen
            case (Some(v1), Some(v2)) => mergeFrameEntry(v1, v2)
            case (None, None) => throw new RuntimeException("should not happen")
          }
        }),
        (this.stack zip that.stack).map(v => mergeFrameEntry(v._1, v._2))
      )
    }
  }

  private def mergeFrameEntry(v1: FrameEntry, v2: FrameEntry): FrameEntry = {
    val mergedInstrs: Set[Instruction] = v1._2 ++ v2._2
    (v1._1, v2._1) match {
      case (a: V_TYPE, b) => (V_TYPE(), mergedInstrs)
      case (a, b: V_TYPE) => (V_TYPE(), mergedInstrs)
      case (a: REF_TYPE, b: V_REF_TYPE) => (b, mergedInstrs)
      case (a: UNINITIALIZED_TYPE, b) => (b, mergedInstrs)
      case (a: V_REF_TYPE, b: V_REF_TYPE) => (b, mergedInstrs) // id could be different
      case _ => {
        if (v1._1 == v2._1) (v1._1, mergedInstrs)
        else throw new RuntimeException("Type mismatch, old: " + v1._1 + " new: " + v2._1)
      }
    }
  }

  private def mapMerge[K, V](m1: Map[K, V], m2: Map[K, V], fun: (Option[V], Option[V]) => V): Map[K, V] =
    (m1.keySet ++ m2.keySet) map { i => i -> fun(m1.get(i), m2.get(i)) } toMap

  /**
    * push to the stack
    */
  def push(v: VBCType, instr: Set[Instruction]): VBCFrame = {
    this.copy(stack = (v, instr) :: stack)
  }

  /**
    * remove all elements from the stack (for exceptions)
    */
  def emptyStack(): VBCFrame = {
    this.copy(stack = Nil)
  }

  /**
    * pop one value from stack
    */
  def pop(): (VBCType, Set[Instruction], VBCFrame) = {
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
