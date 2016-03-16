package edu.cmu.cs.vbc.analysis

import edu.cmu.cs.vbc.vbytecode.instructions.{Instruction, JumpInstruction}
import edu.cmu.cs.vbc.vbytecode.{MethodEnv, VBCMethodNode}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type

/**
  * Compute frames before and after bytecode execution
  *
  * @author chupanw
  */
class VBCAnalyzer(env: MethodEnv) {

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
    * An array of frames before executing each instruction
    */
  val beforeFrames: Option[Array[VBCFrame]] = {
    // we don't compute frames for abstract or native methods
    if ((mn.access & (ACC_ABSTRACT | ACC_NATIVE)) != 0) {
      None
    }
    // init
    val frames: Array[VBCFrame] = new Array(n)
    val queued: Array[Boolean] = new Array(n)
    val queue: Array[Int] = new Array(n)
    var top: Int = 0
    val current: VBCFrame = new VBCFrame(env.maxLocals)
    var local: Int = 0
    // init this
    if ((mn.access & ACC_STATIC) == 0) {
      current.setLocal(local, VBCValue.newValue(Type.getObjectType(env.clazz.name)))
      local += 1
    }
    // init args
    for (t <- Type.getArgumentTypes(mn.desc)) {
      current.setLocal(local, VBCValue.newValue(t))
      local += 1
    }
    // init other local variables
    while (local < env.maxLocals) {
      current.setLocal(local, VBCValue.newValue(null))
      local += 1
    }

    /**
      * Merge the new frame into frames array
      *
      * @param insn
      * @param frame
      */
    def merge(insn: Int, frame: VBCFrame): Unit = {
      val oldFrame = frames(insn)
      var changed = false
      if (oldFrame == null) {
        frames(insn) = new VBCFrame(frame)
        changed = true
      }
      else {
        changed = oldFrame.merge(frame)
      }
      if (changed && !queued(insn)) {
        queued(insn) = true
        queue(top) = insn
        top += 1
      }
    }
    merge(0, current)
    // control flow analysis
    while (top > 0) {
      top -= 1
      val insn: Int = queue(top)
      queued(insn) = false
      val instr: Instruction = env.instructions(insn)
      val current = new VBCFrame(frames(insn))
      current.execute(instr, env)
      if (instr.isJumpInstr) {
        val j = instr.asInstanceOf[JumpInstruction]
        val (uncond, cond) = j.getSuccessor()
        if (cond.isDefined) merge(env.getBlockStart(cond.get), current)
        if (uncond.isDefined) merge(env.getBlockStart(uncond.get), current) else merge(insn + 1, current)
      }
      else if (!instr.isReturnInstr) {
        merge(insn + 1, current)
      }
    }
    Some(frames)
  }

  val afterFrames: Option[Array[VBCFrame]] = {
    if (beforeFrames.isDefined) {
      val frames: Array[VBCFrame] = new Array[VBCFrame](beforeFrames.get.length)
      for (i <- frames.indices)
        frames(i) = new VBCFrame(beforeFrames.get(i))
      (frames zip instructions).foreach((pair) => pair._1.execute(pair._2, env))
      Some(frames)
    }
    else
      None
  }
}

