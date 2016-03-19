package edu.cmu.cs.vbc.analysis

import edu.cmu.cs.vbc.analysis.VBCFrame.StackEntry
import edu.cmu.cs.vbc.util.LiftingFilter
import edu.cmu.cs.vbc.vbytecode.instructions._
import edu.cmu.cs.vbc.vbytecode.{VMethodEnv, Variable}
import org.objectweb.asm.Type

object VBCFrame {
  //an entry consists of the type of the entry and all instructions that are possibly responsible for setting this entry
  type StackEntry = (VBCValue, Set[Instruction])

  //merge two entries by merging their types and combining their responsible instructions
  def merge(v1: StackEntry, v2: StackEntry): StackEntry =
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
  def write(v: Variable, vtype: VBCValue, instr: Instruction): VBCFrame =
    this.copy(localVar = localVar + (v ->(vtype, Set(instr))))

  /**
    * shorthand for a store operation (pop and write)
    */
  def store(variable: Variable, vtype: VBCValue, instr: Instruction): VBCFrame = {
    val (foundVtype, _, newFrame) = this.pop()
    assert(foundVtype == vtype, s"expected $vtype but found $foundVtype on stack")
    newFrame.write(variable, vtype, instr)
  }

  /**
    * shorthand for a load operation (read local and push)
    */
  def load(variable: Variable, vtype: VBCValue, instr: Instruction): VBCFrame = {
    assert(localVar contains variable, "variable not assigned previously")
    var (foundVtype, _) = localVar(variable)
    assert(foundVtype == vtype, s"expected $vtype but found $foundVtype as local variable")
    this.push(vtype, instr)
  }


  //  /**
  //    * Merge two frames, for each variables, set the value to TOP if differs
  //    *
  //    * @param frame
  //    * @return
  //    */
  //  def merge(frame: VBCFrame): Boolean = {
  //    if (this.localVar.size != frame.localVar.size || this.stack.size != frame.stack.size) {
  //      throw new RuntimeException("Incompatible stack heights")
  //    }
  //    var changed: Boolean = false
  //    for (i <- 0 until localVar.size) {
  //      val merged = VBCFrame.merge(localVar(i), frame.localVar(i))
  //      changed = merged != values(i) | changed
  //      if (changed) values(i) = merged
  //    }
  ////    changed = changed | isInCtx != frame.isInCtx
  //    changed
  //  }

  /**
    * push to the stack
    *
    * @param v
    */
  def push(v: VBCValue, instr: Instruction): VBCFrame =
    this.copy(stack = (v, Set(instr)) :: stack)


  /**
    * pop one value from stack
    *
    * @return
    */
  def pop(): (VBCValue, Set[Instruction], VBCFrame) = {
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
    localVar.map(_._1).mkString + " " + stack.reverse.map(_._1).mkString

}
