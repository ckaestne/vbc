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

  "LCMP" can "compare VLong(equal and bigger) with long" in {
    methodWithBlocks(
      createVlong(tValue = 1, fValue = 2, startBlockIdx = 0) :::
      Block(InstrLDC(new java.lang.Long(1)), InstrLCMP(), InstrDBGIPrint(), InstrRETURN()) ::
      Nil
    )
  }

  it can "compare VLong(smaller and bigger) with long" in {
    methodWithBlocks(
      createVlong(tValue = 1, fValue = 3, startBlockIdx = 0) :::
      Block(InstrLDC(new java.lang.Long(2)), InstrLCMP(), InstrDBGIPrint(), InstrRETURN()) ::
      Nil
    )
  }

  it can "compare VLong with VLong" in {
    methodWithBlocks(
      createVlong(tValue = 1, fValue = 3, startBlockIdx = 0, config = "A") :::
      createVlong(tValue = 0, fValue = 2, startBlockIdx = 3, config = "B")  :::
      Block(InstrLCMP(), InstrDBGIPrint(), InstrRETURN()) ::
      Nil
    )
  }

  it should "throw exception if values being compared are not long" in {
    assertThrows[Throwable] {
      methodWithBlocks(
        createVlong(tValue = 1, fValue = 2, startBlockIdx = 0) :::
        Block(InstrICONST(1), InstrLCMP(), InstrDBGIPrint(), InstrRETURN()) ::
        Nil
      )
    }
  }
}
