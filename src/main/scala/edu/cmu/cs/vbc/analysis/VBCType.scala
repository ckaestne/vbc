package edu.cmu.cs.vbc.analysis

import org.objectweb.asm.Type

/**
  * Symbolic value for interpretation of bytecode
  *
  * Lattice:
  *
  * V_TYPE
  * /     \
  * /      V_REF_TYPE
  * /        \
  * INT_TYPE     REF_TYPE
  * \        /
  * \       /
  * UNINITIALIZED_TYPE
  *
  * @todo some other basic types (float, long, double)
  * @author chupanw
  */
sealed abstract class VBCType

case class INT_TYPE() extends VBCType {
  override def toString: String = "I"
}

case class V_TYPE() extends VBCType {
  override def toString: String = "V"
}

case class REF_TYPE(isException: Boolean = false) extends VBCType {
  override def toString: String = "R"
}


/**
  * Represents reference that created by NEW instruction
  *
  * @param id used to distinguish object, potentially there could be a lot of different new object references on stack
  */
case class V_REF_TYPE(id: Int) extends VBCType {
  override def toString: String = "N"
}

case class UNINITIALIZED_TYPE() extends VBCType {
  override def toString: String = "?"
}

object VBCType {
  /**
    * Create a new value based on type
    *
    * @param t
    * @return
    */
  def apply(t: Type): VBCType = t match {
    case null => UNINITIALIZED_TYPE()
    case _ => {
      t.getSort match {
        case Type.BOOLEAN | Type.CHAR | Type.BYTE | Type.SHORT | Type.INT => INT_TYPE()
        case Type.OBJECT | Type.ARRAY => REF_TYPE()
        case _ => throw new RuntimeException("Type " + t + " is not supported yet")
      }
    }
  }

  def merge(v1: VBCType, v2: VBCType): VBCType = v2

  var id = 0

  def nextID: Int = {
    id += 1
    id - 1
  }
}
