package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.vbytecode.Block
import edu.cmu.cs.vbc.{DiffMethodTestInfrastructure, InstrDBGIPrint}
import org.scalatest.FlatSpec

/**
  * @author chupanw
  */
class ArithmeticInstructionsTest extends FlatSpec with DiffMethodTestInfrastructure {
  "ISHL" can "shift left int value" in {
    methodWithBlocks(
      createVint(tValue = 1, fValue = 2, 0) :::
      Block(InstrICONST(1), InstrISHL(), InstrDBGIPrint(), InstrRETURN()) ::
      Nil
    )
  }
}
