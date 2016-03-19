package edu.cmu.cs.vbc.analysis

import org.objectweb.asm.Type

/**
  * Symbolic value for interpretation of bytecode
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

case class REF_TYPE() extends VBCType {
  override def toString: String = "R"
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

}
