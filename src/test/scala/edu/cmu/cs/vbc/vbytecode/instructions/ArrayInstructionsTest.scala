package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.vbytecode.{Block, Owner}
import edu.cmu.cs.vbc.{DiffMethodTestInfrastructure, InstrDBGIPrint}
import org.objectweb.asm.Opcodes._
import org.scalatest.FlatSpec

/**
  * @author chupanw
  */
class ArrayInstructionsTest extends FlatSpec with DiffMethodTestInfrastructure {
  "ARRAYLENGTH" can "get the length of primitive array" in {
    methodWithBlocks(
      createVPrimitiveArray(T_INT, 0, 10, 20) :::
      Block(InstrARRAYLENGTH(), InstrDBGIPrint(), InstrRETURN()) ::
      Nil
    )
  }

  it can "get the length of Integer array" in {
    methodWithBlocks(
      createVObjectArray(Owner("java/lang/Integer"), 0, 20, 30) :::
      Block(InstrARRAYLENGTH(), InstrDBGIPrint(), InstrRETURN()) ::
      Nil
    )
  }
}
