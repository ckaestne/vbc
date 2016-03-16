package edu.cmu.cs.vbc.analysis

import edu.cmu.cs.vbc.test._
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
    top = srcFrame.top
  }

  /**
    * Set the value for local variable with index i to value
    *
    * @param i
    * index
    * @param value
    * new value to set to
    */
  def setLocal(i: Int, value: VBCValue) = {
    values(i) = value
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
    def push(v: VBCValue): Unit = {
      if (top + nLocals >= values.length)
        throw new IndexOutOfBoundsException("Insufficient maxium stack size")
      values(top + nLocals) = v
      top += 1
    }

    /**
      * pop one value from stack
      *
      * @return
      */
    def pop(): VBCValue = {
      if (top == 0)
        throw new IndexOutOfBoundsException("Cannot pop operand off an empty stack.")
      top -= 1
      val ret = values(top + nLocals)
      values(nLocals + top) = null
      ret
    }

    instr match {
      case i: InstrACONST_NULL => push(VBCValue.newValue(Type.getObjectType("null")))
      case i: InstrICONST => push(INT_TYPE())
      case i: InstrBIPUSH => push(INT_TYPE())
      case i: InstrSIPUSH => push(INT_TYPE())
      case i: InstrLDC => {
        i.o match {
          case integer: java.lang.Integer => push(INT_TYPE())
          case string: java.lang.String => push(VBCValue.newValue(Type.getObjectType("java/lang/String")))
          case _ => throw new RuntimeException("Incomplete support for LDC")
        }
      }
      case i: InstrILOAD => push(values(env.getVarIdxNoCtx(i.variable)))
      case i: InstrALOAD => push(values(env.getVarIdxNoCtx(i.variable)))
      case i: InstrISTORE => {
        val old = pop()
        setLocal(env.getVarIdxNoCtx(i.variable), old) //TODO: float and double
      }
      case i: InstrASTORE => {
        val old = pop()
        setLocal(env.getVarIdxNoCtx(i.variable), old)
      }
      case i: InstrPOP => pop()
      case i: InstrDUP => {
        val v = pop(); push(v); push(v)
      }
      case i: InstrIADD => {
        pop(); pop(); push(INT_TYPE())
      }
      case i: InstrISUB => {
        pop(); pop(); push(INT_TYPE())
      }
      case i: InstrIMUL => {
        pop(); pop(); push(INT_TYPE())
      }
      case i: InstrIDIV => {
        pop(); pop(); push(INT_TYPE())
      }
      case i: InstrIINC => setLocal(env.getVarIdxNoCtx(i.variable), INT_TYPE())
      case i: InstrIFEQ => pop()
      case i: InstrIFNE => pop()
      case i: InstrIFGE => pop()
      case i: InstrIFGT => pop()
      case i: InstrIF_ICMPEQ => {
        pop(); pop()
      }
      case i: InstrIF_ICMPNE => {
        pop(); pop()
      }
      case i: InstrIF_ICMPLT => {
        pop(); pop()
      }
      case i: InstrIF_ICMPGE => {
        pop(); pop()
      }
      case i: InstrGOTO => // does not affect locals and stack
      case i: InstrIRETURN => pop()
      case i: InstrARETURN => pop()
      case i: InstrRETURN => // do nothing
      case i: InstrGETSTATIC => push(VBCValue.newValue(Type.getType(i.desc)))
      case i: InstrPUTSTATIC => pop()
      case i: InstrGETFIELD => {
        pop(); push(VBCValue.newValue(Type.getType(i.desc)))
      }
      case i: InstrPUTFIELD => {
        pop(); pop()
      }
      case i: InstrINVOKEVIRTUAL => {
        pop() // L0
        for (i <- 0 until Type.getArgumentTypes(i.desc).length) pop()
        if (Type.getReturnType(i.desc) != Type.VOID_TYPE)
          push(VBCValue.newValue(Type.getReturnType(i.desc)))
      }
      case i: InstrINVOKESTATIC => {
        for (i <- 0 until Type.getArgumentTypes(i.desc).length) pop()
        if (Type.getReturnType(i.desc) != Type.VOID_TYPE)
          push(VBCValue.newValue(Type.getReturnType(i.desc)))
      }
      case i: InstrINVOKESPECIAL => {
        pop() // L0
        for (i <- 0 until Type.getArgumentTypes(i.desc).length) pop()
        if (Type.getReturnType(i.desc) != Type.VOID_TYPE)
          push(VBCValue.newValue(Type.getReturnType(i.desc)))
      }
      case i: InstrNEW => {
        push(VBCValue.newValue(Type.getObjectType(i.t)))
      }
      case i: InstrINIT_CONDITIONAL_FIELDS => // does not change stack
      case i: InstrDBGIPrint => pop()
      case i: InstrLoadConfig => push(INT_TYPE())
      case i: InstrDBGCtx => // does not change stack
      case i: TraceInstr_ConfigInit => // does not change stack
      case i: TraceInstr_S => // does not change stack
      case i: TraceInstr_Print => // does not change stack
      case i: TraceInstr_GetField => // does not change stack
      case _ => throw new RuntimeException("Instruction " + instr + " is not supported yet")
    }
  }

  override def toString: String = {
    val sb = new StringBuilder
    values.take(nLocals).foreach(sb.append(_))
    sb.append(' ')
    values.drop(nLocals).foreach((v) => if (v != null) sb.append(v) else sb.append("."))
    sb.toString()
  }

  def getStackSize = top
}
