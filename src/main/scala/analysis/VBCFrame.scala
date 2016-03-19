package edu.cmu.cs.vbc.analysis

import edu.cmu.cs.vbc.analysis.VBCFrame.StackEntry
import edu.cmu.cs.vbc.vbytecode.Variable
import edu.cmu.cs.vbc.vbytecode.instructions._

object VBCFrame {
  //an entry consists of the type of the entry and all instructions that are possibly responsible for setting this entry
  type StackEntry = (VBCType, Set[Instruction])

  //merge two entries by merging their types and combining their responsible instructions
  def mergeEntry(v1: StackEntry, v2: StackEntry): StackEntry =
    (if (v1._1 != v2._1) UNINITIALIZED_TYPE() else v1._1, v1._2 ++ v2._2)

}

/**
  * For each instruction, Frame contains information about local variables and stack elements.
  *
  * Local variables and stacks both store the type of the entry and the instruction that last updated
  * the entry (in case one wants to later lift the operation that creates this entry)
  *
  */
case class VBCFrame(
                       localVar: Map[Variable, StackEntry],
                       stack: List[StackEntry]) {


  /**
    * Set the value for local variable with index i to value
    */
  def write(v: Variable, vtype: VBCType, instr: Instruction): VBCFrame =
    this.copy(localVar = localVar + (v ->(vtype, Set(instr))))

  /**
    * shorthand for a store operation (pop and write)
    */
  def store(variable: Variable, vtype: VBCType, instr: Instruction): VBCFrame = {
    val (foundVtype, _, newFrame) = this.pop()
    assert(foundVtype == vtype, s"expected $vtype but found $foundVtype on stack")
    newFrame.write(variable, vtype, instr)
  }

  /**
    * shorthand for a load operation (read local and push)
    */
  def load(variable: Variable, vtype: VBCType, instr: Instruction): VBCFrame = {
    assert(localVar contains variable, "variable not assigned previously")
    var (foundVtype, _) = localVar(variable)
    assert(foundVtype == vtype, s"expected $vtype but found $foundVtype as local variable")
    this.push(vtype, instr)
  }


  /**
    * Merge two frames, for each variables, set the type to TOP if differs
    */
  def merge(that: VBCFrame): VBCFrame =
    if (this == that)
      this
    else if (this.localVar.keySet != that.localVar.keySet || this.stack.size != that.stack.size)
      throw new RuntimeException("Incompatible stack heights")
    else
      VBCFrame(
        mapMerge(this.localVar, that.localVar, VBCFrame.mergeEntry),
        (this.stack zip that.stack).map(v => VBCFrame.mergeEntry(v._1, v._2))
      )


  private def mapMerge[K, V](m1: Map[K, V], m2: Map[K, V], fun: (V, V) => V): Map[K, V] =
    (m1.keySet ++ m2.keySet) map { i => i -> ((m1.get(i), m2.get(i)) match {
      case (None, Some(v)) => v
      case (Some(v), None) => v
      case (Some(v1), Some(v2)) => fun(v1, v2)
    })
    } toMap


  /**
    * push to the stack
    *
    * @param v
    */
  def push(v: VBCType, instr: Instruction): VBCFrame =
    this.copy(stack = (v, Set(instr)) :: stack)


  /**
    * pop one value from stack
    *
    * @return
    */
  def pop(): (VBCType, Set[Instruction], VBCFrame) = {
    if (stack.isEmpty)
      throw new IndexOutOfBoundsException("Cannot pop operand off an empty stack.")
    val entry = stack.head
    (entry._1, entry._2, this.copy(stack = stack.tail))
  }

  /**
    * Execute one instruction
    */
//  def execute(instr: Instruction, env: VMethodEnv): Unit = {





//      case i: InstrDBGIPrint => pop()
//      case i: InstrLoadConfig => push(INT_TYPE(), i::Nil)
//      case i: InstrDBGCtx => // does not change stack
//      case i: TraceInstr_ConfigInit => // does not change stack
//      case i: TraceInstr_S => // does not change stack
//      case i: TraceInstr_Print => // does not change stack
//      case i: TraceInstr_GetField => // does not change stack
//      case _ => throw new RuntimeException("Instruction " + instr + " is not supported yet")
//    }
//  }

  override def toString: String =
    localVar.map(l => l._1 + "-" + l._2._1).mkString + " " + stack.reverse.map(_._1).mkString

}
