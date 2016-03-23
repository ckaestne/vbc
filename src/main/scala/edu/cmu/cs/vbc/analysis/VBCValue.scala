package edu.cmu.cs.vbc.analysis

import org.objectweb.asm.Type

/**
  * Symbolic value for interpretation of bytecode
  *
  * @todo some other basic types (float, long, double)
  * @author chupanw
  */
sealed abstract class VBCValue

case class INT_TYPE() extends VBCValue {
  override def toString: String = "I"
}

case class V_TYPE() extends VBCValue {
  override def toString: String = "V"
}

case class REF_TYPE() extends VBCValue {
  override def toString: String = "R"
}

/**
  * Represents reference that created by NEW instruction
  *
  * @param id used to distinguish object, potentially there could be a lot of different new object references on stack
  */
case class V_REF_TYPE(id: Int) extends VBCValue {
  override def toString: String = "N"
}

case class UNINITIALIZED_TYPE() extends VBCValue {
  override def toString: String = "?"
}

object VBCValue {
  /**
    * Create a new value based on type
    *
    * @param t
    * @return
    */
  def newValue(t: Type): VBCValue = t match {
    case null => UNINITIALIZED_TYPE()
    case _ => {
      t.getSort match {
        case Type.BOOLEAN | Type.CHAR | Type.BYTE | Type.SHORT | Type.INT => INT_TYPE()
        case Type.OBJECT | Type.ARRAY => REF_TYPE()
        case _ => throw new RuntimeException("Type " + t + " is not supported yet")
      }
    }
  }

  def merge(v1: VBCValue, v2: VBCValue): VBCValue = v2

  var id = 0

  def nextID: Int = {
    id += 1
    id - 1
  }
}
