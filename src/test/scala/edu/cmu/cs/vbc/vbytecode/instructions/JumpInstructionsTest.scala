package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.vbytecode.Block
import edu.cmu.cs.vbc.{DiffMethodTestInfrastructure, InstrDBGStrPrint}
import org.scalatest.FlatSpec

/**
  * @author chupanw
  */
class JumpInstructionsTest extends FlatSpec with DiffMethodTestInfrastructure {
  "IFEQ" should "jump if value of stack is zero" in {
    methodWithBlocks(
      createVint(tValue = 0, fValue = 1, startBlockIdx = 0) :::
      Block(InstrIFLE(5)) ::
      Block(InstrLDC("not jump"), InstrDBGStrPrint(), InstrGOTO(6)) ::
      Block(InstrLDC("jump"), InstrDBGStrPrint(), InstrGOTO(6)) ::
      Block(InstrRETURN()) ::
      Nil
    )
  }

  "IFLE" should "jump if value of stack is less than or equal to zero" in {
    methodWithBlocks(
      createVint(tValue = 0, fValue = 1, startBlockIdx = 0) :::
      Block(InstrIFLE(5)) ::
      Block(InstrLDC("not jump"), InstrDBGStrPrint(), InstrGOTO(6)) ::
      Block(InstrLDC("jump"), InstrDBGStrPrint(), InstrGOTO(6)) ::
      Block(InstrRETURN()) ::
      Nil
    )
  }
}
