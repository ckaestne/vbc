package edu.cmu.cs.vbc.analysis

import edu.cmu.cs.vbc.util.LiftingFilter
import edu.cmu.cs.vbc.vbytecode.VMethodEnv
import edu.cmu.cs.vbc.vbytecode.instructions._
import org.objectweb.asm.Type

/**
  * For each instruction, Frame contains information about local variables and stack elements.
  *
  * @param nLocals
  * number of local variables of this frame
  */
class VBCFrame(nLocals: Int) {
  /**
    * Store values of local variables and stack elements
    */
  val values: Array[VBCValue] = new Array(nLocals * 10) // this is just an approximation

  /**
    * If value is updated, also store the instruction chain that updates this value
    */
  val sourceInstrs: Array[List[Instruction]] = new Array(values.length)

  /**
    * Whether the current frame is under context
    */
//  var isInCtx: Boolean = false

  /**
    * The number of elements in the operand stack
    */
  var top = 0

  /**
    * The number of local variables
    */
  val locals = nLocals

  def this(srcFrame: VBCFrame) = {
    this(srcFrame.locals)
    for (i <- 0 until values.length) values(i) = srcFrame.values(i)
    for (i <- 0 until sourceInstrs.length) sourceInstrs(i) = srcFrame.sourceInstrs(i)
    top = srcFrame.top
//    isInCtx = srcFrame.isInCtx
  }

  /**
    * Set the value for local variable with index i to value
    *
    * @param i
    * index
    * @param value
    * new value to set to
    */
  def setLocal(i: Int, value: VBCValue, chain: Option[List[Instruction]]) = {
    values(i) = value
    if (chain.isDefined) {
      sourceInstrs(i) = chain.get
    }
    else
      sourceInstrs(i) = Nil
  }

  /**
    * Merge two frames, for each variables, set the value to TOP if differs
    *
    * @param frame
    * @return
    */
  def merge(frame: VBCFrame): Boolean = {
    if (top != frame.top) {
      throw new RuntimeException("Incompatible stack heights")
    }
    var changed: Boolean = false
    for (i <- 0 until nLocals + top) {
      val merged = VBCValue.merge(values(i), frame.values(i))
      changed = merged != values(i) | changed
      if (changed) values(i) = merged
    }
//    changed = changed | isInCtx != frame.isInCtx
    changed
  }

  /**
    * Execute one instruction
    */
  def execute(instr: Instruction, env: VMethodEnv): Unit = {

    /**
      * push to the stack
      *
      * @param v
      */
    def push(v: VBCValue, chain: List[Instruction]): Unit = {
      if (top + nLocals >= values.length)
        throw new IndexOutOfBoundsException("Insufficient maxium stack size")
      values(top + nLocals) = v
      sourceInstrs(top + nLocals) = chain
      top += 1
    }

    /**
      * pop one value from stack
      *
      * @return
      */
    def pop(): (VBCValue, List[Instruction]) = {
      if (top == 0)
        throw new IndexOutOfBoundsException("Cannot pop operand off an empty stack.")
      top -= 1
      val ret = values(top + nLocals)
      val chain = sourceInstrs(top + nLocals)
      values(nLocals + top) = null
      sourceInstrs(nLocals + top) = Nil
      (ret, chain)
    }

    instr match {
      case i: InstrACONST_NULL => push(VBCValue.newValue(Type.getObjectType("null")), i::Nil)
      case i: InstrICONST => push(INT_TYPE(), i::Nil)
      case i: InstrBIPUSH => push(INT_TYPE(), i::Nil)
      case i: InstrSIPUSH => push(INT_TYPE(), i::Nil)
      case i: InstrLDC => {
        i.o match {
          case integer: java.lang.Integer => push(INT_TYPE(), i::Nil)
          case string: java.lang.String => push(VBCValue.newValue(Type.getObjectType("java/lang/String")), i::Nil)
          case _ => throw new RuntimeException("Incomplete support for LDC")
        }
      }
      case i: InstrILOAD => push(values(env.getVarIdxNoCtx(i.variable)), i::sourceInstrs(env.getVarIdxNoCtx(i.variable)))
      case i: InstrALOAD => push(values(env.getVarIdxNoCtx(i.variable)), i::sourceInstrs(env.getVarIdxNoCtx(i.variable)))
      case i: InstrISTORE => {
        val (old, chain) = pop()
        setLocal(env.getVarIdxNoCtx(i.variable), V_TYPE(), Some(i :: chain)) //TODO: float and double
        env.setChainToTrue(chain)
        env.setInsnToTrue(i)
      }
      case i: InstrASTORE => {
        val (old, chain) = pop()
        setLocal(env.getVarIdxNoCtx(i.variable), V_TYPE(), Some(i :: chain))
        env.setChainToTrue(chain)
        env.setInsnToTrue(i)
      }
      case i: InstrPOP => pop()
      case i: InstrDUP => {
        val (v, chain) = pop(); push(v, chain); push(v, chain)
      }
      case i: InstrIADD => {
        val (v1, chain1) = pop()
        val (v2, chain2) = pop()
        push(INT_TYPE(), i :: chain1 ::: chain2)
      }
      case i: InstrISUB => {
        val (v1, chain1) = pop()
        val (v2, chain2) = pop()
        push(INT_TYPE(), i :: chain1 ::: chain2)
      }
      case i: InstrIMUL => {
        val (v1, chain1) = pop()
        val (v2, chain2) = pop()
        push(INT_TYPE(), i :: chain1 ::: chain2)
      }
      case i: InstrIDIV => {
        val (v1, chain1) = pop()
        val (v2, chain2) = pop()
        push(INT_TYPE(), i :: chain1 ::: chain2)
      }
      case i: InstrIINC => {
        setLocal(env.getVarIdxNoCtx(i.variable), V_TYPE(), Some(i :: sourceInstrs(env.getVarIdxNoCtx(i.variable))))
        env.setChainToTrue(sourceInstrs(env.getVarIdxNoCtx(i.variable)))
        env.setInsnToTrue(i)
      }
      case i: InstrIFEQ => {
        val (v, chain) = pop()
        if (v.isInstanceOf[V_TYPE]) {
          env.setChainToTrue(chain)
          env.setInsnToTrue(i)
        }
      }
      case i: InstrIFNE => {
        val (v, chain) = pop()
        if (v.isInstanceOf[V_TYPE]) {
          env.setChainToTrue(chain)
          env.setInsnToTrue(i)
        }
      }
      case i: InstrIFGE => {
        val (v, chain) = pop()
        if (v.isInstanceOf[V_TYPE]) {
          env.setChainToTrue(chain)
          env.setInsnToTrue(i)
        }
      }
      case i: InstrIFGT => {
        val (v, chain) = pop()
        if (v.isInstanceOf[V_TYPE]) {
          env.setChainToTrue(chain)
          env.setInsnToTrue(i)
        }
      }
      case i: InstrIF_ICMPEQ => {
        val (v1, chain1) = pop()
        val (v2, chain2) = pop()
        if (v1.isInstanceOf[V_TYPE] || v2.isInstanceOf[V_TYPE]) {
          env.setChainToTrue(chain1)
          env.setChainToTrue(chain2)
          env.setInsnToTrue(i)
        }
      }
      case i: InstrIF_ICMPNE => {
        val (v1, chain1) = pop()
        val (v2, chain2) = pop()
        if (v1.isInstanceOf[V_TYPE] || v2.isInstanceOf[V_TYPE]) {
          env.setChainToTrue(chain1)
          env.setChainToTrue(chain2)
          env.setInsnToTrue(i)
        }
      }
      case i: InstrIF_ICMPLT => {
        val (v1, chain1) = pop()
        val (v2, chain2) = pop()
        if (v1.isInstanceOf[V_TYPE] || v2.isInstanceOf[V_TYPE]) {
          env.setChainToTrue(chain1)
          env.setChainToTrue(chain2)
          env.setInsnToTrue(i)
        }
      }
      case i: InstrIF_ICMPGE => {
        val (v1, chain1) = pop()
        val (v2, chain2) = pop()
        if (v1.isInstanceOf[V_TYPE] || v2.isInstanceOf[V_TYPE]) {
          env.setChainToTrue(chain1)
          env.setChainToTrue(chain2)
          env.setInsnToTrue(i)
        }
      }
      case i: InstrGOTO => // does not affect locals and stack
      case i: InstrIRETURN => {
        // assuming all methods return V
        val (v, chain) = pop()
        env.setChainToTrue(chain)
        env.setInsnToTrue(i)
      }
      case i: InstrARETURN => {
        // assuming all methods return V
        val (v, chain) = pop()
        env.setChainToTrue(chain)
        env.setInsnToTrue(i)
      }
      case i: InstrRETURN => // do nothing
      case i: InstrGETSTATIC => push(V_TYPE(), i::Nil) // all fields are V
      case i: InstrPUTSTATIC => {
        val (v, chain) = pop()
        env.setChainToTrue(chain)
        env.setInsnToTrue(i)
      }
      case i: InstrGETFIELD => {
        val (v, chain) = pop()
        push(V_TYPE(), i::chain)
        env.setChainToTrue(chain)
        env.setInsnToTrue(i)
      }
      case i: InstrPUTFIELD => {
        val (v1, chain1) = pop()
        val (v2, chain2) = pop()
        env.setChainToTrue(chain1)
        env.setChainToTrue(chain2)
        env.setInsnToTrue(i)
      }
      case i: InstrINVOKEVIRTUAL => {
        val (v, chain) = pop() // L0
        env.setChainToTrue(chain)
        env.setInsnToTrue(i)
        for (j <- 0 until Type.getArgumentTypes(i.desc).length){
          val (vi, chaini) = pop()
          if (LiftingFilter.shouldLiftMethod(i.owner, i.name, i.desc)) {
            env.setChainToTrue(chaini)
            env.setInsnToTrue(i)
          }
        }
        if (Type.getReturnType(i.desc) != Type.VOID_TYPE)
          push(V_TYPE(), i::Nil)
      }
      case i: InstrINVOKESTATIC => {
        for (j <- 0 until Type.getArgumentTypes(i.desc).length){
          val (v, chain) = pop()
          if (LiftingFilter.shouldLiftMethod(i.owner, i.name, i.desc)) {
            env.setChainToTrue(chain)
            env.setInsnToTrue(i)
          }
        }
        if (Type.getReturnType(i.desc) != Type.VOID_TYPE)
          push(V_TYPE(), i::Nil)
      }
      case i: InstrINVOKESPECIAL => {
        val (v, chain) = pop() // L0
        for (j <- 0 until Type.getArgumentTypes(i.desc).length){
          val (vi, chaini) = pop()
          if (LiftingFilter.shouldLiftMethod(i.owner, i.name, i.desc)) {
            env.setChainToTrue(chaini)
            env.setInsnToTrue(i)
          }
        }
        if (Type.getReturnType(i.desc) != Type.VOID_TYPE)
          push(V_TYPE(), i::Nil)
      }
      case i: InstrNEW => {
        push(VBCValue.newValue(Type.getObjectType(i.t)), i::Nil)
      }
      case i: InstrINIT_CONDITIONAL_FIELDS => // does not change stack
//      case i: InstrDBGIPrint => pop()
//      case i: InstrLoadConfig => push(INT_TYPE(), i::Nil)
//      case i: InstrDBGCtx => // does not change stack
//      case i: TraceInstr_ConfigInit => // does not change stack
//      case i: TraceInstr_S => // does not change stack
//      case i: TraceInstr_Print => // does not change stack
//      case i: TraceInstr_GetField => // does not change stack
      case _ => throw new RuntimeException("Instruction " + instr + " is not supported yet")
    }
  }

  override def toString: String = {
    val sb = new StringBuilder
//    if (isInCtx) sb.append("[x]") else sb.append("[ ]")
    values.take(nLocals).foreach(sb.append(_))
    sb.append(' ')
    values.drop(nLocals).foreach((v) => if (v != null) sb.append(v) else sb.append("."))
    sb.toString()
  }

  def getStackSize = top
}
