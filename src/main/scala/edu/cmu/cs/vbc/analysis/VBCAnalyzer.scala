package edu.cmu.cs.vbc.analysis

import edu.cmu.cs.vbc.vbytecode.instructions.{Instruction, JumpInstruction}
import edu.cmu.cs.vbc.vbytecode.{Parameter, VBCMethodNode, VMethodEnv}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type

import scala.collection.mutable.Queue

/**
  * Compute frames before and after bytecode execution
  *
  * @author chupanw
  */
class VBCAnalyzer(env: VMethodEnv) {

  /**
    * Method node
    */
  val mn: VBCMethodNode = env.method
  /**
    * All the instructions in the current method node
    */
  val instructions = env.instructions
  /**
    * Number of instructions
    */
  val n: Int = instructions.length
  /**
    * A map of frames before executing each instruction
    */
  def computeBeforeFrames: Array[VBCFrame] = {
    // we don't compute frames for abstract or native methods
    if ((mn.access & (ACC_ABSTRACT | ACC_NATIVE)) != 0) {
      Map()
    }
    // init
    val beforeInstructionFrames: Array[VBCFrame] = new Array(instructions.size)
    val queue: Queue[Int] = Queue()
    var initialFrame: VBCFrame = new VBCFrame(Map(), Nil)
    var local: Int = 0
    // init this
    if (!env.method.isStatic) {
      initialFrame = initialFrame.setLocal(env.thisParameter, VBCType(Type.getObjectType(env.clazz.name)), Set.empty[Instruction])
    }
    // init args
    val args = Type.getArgumentTypes(env.method.desc)
    for (argIdx <- 0 until args.size) {
      initialFrame = initialFrame.setLocal(new Parameter(if (env.method.isStatic) argIdx else argIdx + 1), VBCType(args(argIdx)), Set.empty[Instruction])
    }

    def getInstr(insn: Int) = instructions(insn)

    /**
      * Merge the new frame into frames array
      */
    def updateFrameForInstr(insn: Int, frame: VBCFrame): Unit = {
      val oldFrame = beforeInstructionFrames(insn)
      var changed = false
      var result: VBCFrame = frame
      if (oldFrame == null) {
        changed = true
      }
      else if (oldFrame != frame) {
        result = oldFrame.merge(frame)
        changed = true
      }

      if (changed && !(queue contains insn)) {
        queue.enqueue(insn)
      }
      beforeInstructionFrames(insn) = result
    }

    beforeInstructionFrames(0) = initialFrame
    queue.enqueue(0)
    // control flow analysis
    while (queue.nonEmpty) {
      val insn: Int = queue.dequeue()
      val instr: Instruction = env.instructions(insn)
      val (frame, backtrack) = instr.updateStack(beforeInstructionFrames(insn), env)
      if (backtrack.nonEmpty) {
        val idxSet: Set[Int] = backtrack.map(env.getInsnIdx)
        backtrack.foreach(_.doBacktrack(env))
        /* Put the current instruction back because backtracking does not guarantee revisit of this instruction */
        /* (if at some point merging two frames does not change, it stops scanning forward) */
        if (!(queue contains insn)) {
          queue.enqueue(insn)
        }
        idxSet.foreach(
          idx => if (!(queue contains idx)) queue.enqueue(idx)
        )
      }
      else {
        instr match {
          case jump: JumpInstruction =>
            val (uncond, cond) = jump.getSuccessor()
            if (cond.isDefined) updateFrameForInstr(env.getBlockStart(cond.get), frame)
            if (uncond.isDefined)
              updateFrameForInstr(env.getBlockStart(uncond.get), frame)
            else
              updateFrameForInstr(insn + 1, frame)
          case i if !i.isReturnInstr =>
            updateFrameForInstr(insn + 1, frame)
          case _ =>
        }
      }
    }
    assert(!beforeInstructionFrames.contains(null), "Missing some frames")
    beforeInstructionFrames
  }

  def computeAfterFrames(beforeFrames: Array[VBCFrame]): Array[VBCFrame] = {
    val array: Array[VBCFrame] = new Array(beforeFrames.size)
    0 until array.size foreach {
      i => array(i) = instructions(i).updateStack(beforeFrames(i), env)._1
    }
    array
  }
}

